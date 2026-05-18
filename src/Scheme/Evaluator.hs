{- | Evaluator: 'Expr' -> 'Value' in the 'Eval' monad.

Responsibility is deliberately limited to @Expr -> Value@: parsing and
analysis live upstream in 'Scheme.Parser' and 'Scheme.Analyzer', and
the end-to-end @Text -> Value@ driver lives in 'Scheme.Interpreter'.
-}
module Scheme.Evaluator (
    applyMacro,
    evaluate,
    evaluateDefine,
    quoteToValue,
) where

import Control.Concurrent (yield)
import Control.Monad.Except (throwError)
import Scheme.AST (
    Body (Body),
    CondClauses (CondClauses, CondElseOnly),
    Define (Define),
    Expr (
        EAnd,
        EApp,
        EBegin,
        ECond,
        EDo,
        EIf,
        ELambda,
        ELet,
        ELetStar,
        ELetrec,
        ELit,
        EOr,
        EQuote,
        ESet,
        EVar
    ),
    Id,
    Literal (LBool, LNil, LNum, LStr),
    Params (Params),
 )
import Scheme.Runtime (
    Env,
    Eval,
    EvalError (ArityError, NotAProcedure),
    Value (VBool, VBuiltin, VClosure, VMacro, VNil, VNum, VPair, VStr, VSym, VUnspecified),
    bindInFrame,
    bindUninitializedInFrame,
    defineUninitializedVar,
    defineVar,
    lookupVar,
    newChildFrame,
    setVar,
    showValueKind,
 )
import Scheme.SExpr (SExpr (SBool, SNil, SNum, SPair, SStr, SSym))

data EvalStep
    = StepDone Value
    | StepExpr Env Expr
    | StepBody Env Body
    | StepSequence Env [Expr]
    | StepApply Value [Value]
    | StepDo Env Env [(Id, Expr, Expr)] (Expr, [Expr]) Body

-- | Evaluate a single expression in the 'Eval' monad.
evaluate :: Expr -> Eval Value
evaluate expr = do
    env <- ask
    drive (StepExpr env expr)

drive :: EvalStep -> Eval Value
drive step = do
    liftIO yield
    case step of
        StepDone value -> pure value
        StepExpr env expr -> local (const env) (evalTail expr) >>= drive
        StepBody env body -> local (const env) (evalBodyTail body) >>= drive
        StepSequence env exprs -> local (const env) (evalSequenceTail exprs) >>= drive
        StepApply procedure args -> applyTail procedure args >>= drive
        StepDo parentEnv iterationEnv bindings test body ->
            local (const iterationEnv) (evalDoIterationTail parentEnv iterationEnv bindings test body) >>= drive

evalTail :: Expr -> Eval EvalStep
evalTail expr = case expr of
    ELit lit -> pure $ StepDone (literalToValue lit)
    EVar ident -> StepDone <$> lookupVar ident
    ELambda params body -> StepDone . VClosure params body <$> ask
    EApp fExpr argExprs -> do
        -- Function and arguments are evaluated left-to-right, following
        -- 'traverse's sequencing in 'Eval'.
        f <- evaluate fExpr
        args <- traverse evaluate argExprs
        pure $ StepApply f args
    EQuote sexpr -> StepDone <$> quoteToValue sexpr
    ESet ident rhs -> do
        value <- evaluate rhs
        setVar ident value
        pure $ StepDone VUnspecified
    ELet name bindings body -> evalLetTail name bindings body
    ELetStar bindings body -> evalLetStarTail bindings body
    ELetrec bindings body -> evalLetrecTail bindings body
    EIf cond thenExpr elseExpr -> evalIfTail cond thenExpr elseExpr
    ECond clauses -> evalCondTail clauses
    EAnd exprs -> evalAndTail exprs
    EOr exprs -> evalOrTail exprs
    EBegin exprs -> evalSequenceTail exprs
    EDo bindings test body -> evalDoTail bindings test body

-- | Evaluate a top-level definition in the current environment.
evaluateDefine :: Define -> Eval Value
evaluateDefine (Define ident rhs) = do
    value <- evaluate rhs
    defineVar ident value
    pure VUnspecified

literalToValue :: Literal -> Value
literalToValue lit = case lit of
    LNum n -> VNum n
    LBool b -> VBool b
    LStr s -> VStr s
    LNil -> VNil

applyTail :: Value -> [Value] -> Eval EvalStep
applyTail (VBuiltin _ f) args = StepDone <$> f args
applyTail (VClosure params body closureEnv) args = do
    callEnv <- liftIO $ newChildFrame closureEnv
    bindParams params args callEnv
    pure $ StepBody callEnv body
applyTail v _ = throwError $ NotAProcedure (showValueKind v)

-- | Apply a macro transformer to already-quoted syntax arguments.
applyMacro :: Value -> [Value] -> Eval Value
applyMacro (VMacro params body closureEnv) args = do
    callEnv <- liftIO $ newChildFrame closureEnv
    bindParams params args callEnv
    local (const callEnv) (evalBody body)
applyMacro v _ = throwError $ NotAProcedure (showValueKind v)

bindParams :: Params -> [Value] -> Env -> Eval ()
bindParams (Params fixed restArg) args frame = do
    let fixedCount = length fixed
        argCount = length args
        (fixedValues, restValues) = splitAt fixedCount args
    when (argCount < fixedCount) $
        throwArityError
            ( "closure: expected at least "
                <> show fixedCount
                <> " arguments, got "
                <> show argCount
            )
    case restArg of
        Nothing -> do
            when (argCount > fixedCount) $
                throwArityError
                    ( "closure: expected "
                        <> show fixedCount
                        <> " arguments, got "
                        <> show argCount
                    )
            bindFixed fixed fixedValues frame
        Just restIdent -> do
            bindFixed fixed fixedValues frame
            restList <- listToValue restValues
            liftIO $ bindInFrame restIdent restList frame

throwArityError :: Text -> Eval a
throwArityError = throwError . ArityError

bindFixed :: [Id] -> [Value] -> Env -> Eval ()
bindFixed names values frame =
    for_ (zip names values) $ \(name, value) ->
        liftIO $ bindInFrame name value frame

evalLetTail :: Maybe Id -> [(Id, Expr)] -> Body -> Eval EvalStep
evalLetTail Nothing bindings body = do
    env <- ask
    evaluatedBindings <- traverse evalBinding bindings
    child <- liftIO $ newChildFrame env
    for_ evaluatedBindings $ \(ident, value) ->
        liftIO $ bindInFrame ident value child
    pure $ StepBody child body
evalLetTail (Just name) bindings body = do
    env <- ask
    child <- liftIO $ newChildFrame env
    let params = Params (fmap fst bindings) Nothing
        closure = VClosure params body child
    liftIO $ bindInFrame name closure child
    args <- traverse (evaluate . snd) bindings
    pure $ StepApply closure args

evalBinding :: (Id, Expr) -> Eval (Id, Value)
evalBinding (ident, rhs) = do
    value <- evaluate rhs
    pure (ident, value)

evalLetStarTail :: [(Id, Expr)] -> Body -> Eval EvalStep
evalLetStarTail bindings body = do
    env <- ask
    base <- liftIO $ newChildFrame env
    go base bindings
  where
    go :: Env -> [(Id, Expr)] -> Eval EvalStep
    go env [] = pure $ StepBody env body
    go env ((ident, rhs) : rest) = do
        value <- local (const env) (evaluate rhs)
        child <- liftIO $ newChildFrame env
        liftIO $ bindInFrame ident value child
        go child rest

evalLetrecTail :: [(Id, Expr)] -> Body -> Eval EvalStep
evalLetrecTail bindings body = do
    env <- ask
    child <- liftIO $ newChildFrame env
    for_ bindings $ \(ident, _) ->
        liftIO $ bindUninitializedInFrame ident child
    local (const child) $ do
        evaluatedBindings <- traverse evalBinding bindings
        for_ evaluatedBindings $ uncurry setVar
        pure $ StepBody child body

evalIfTail :: Expr -> Expr -> Maybe Expr -> Eval EvalStep
evalIfTail cond thenExpr elseExpr = do
    condValue <- evaluate cond
    env <- ask
    if truthy condValue
        then pure $ StepExpr env thenExpr
        else pure $ maybe (StepDone VUnspecified) (StepExpr env) elseExpr

evalCondTail :: CondClauses -> Eval EvalStep
evalCondTail (CondElseOnly body) = evalNonEmptyTail body
evalCondTail (CondClauses (firstClause :| restClauses) elseClause) =
    go (firstClause : restClauses)
  where
    go :: [(Expr, NonEmpty Expr)] -> Eval EvalStep
    go [] = maybe (pure $ StepDone VUnspecified) evalNonEmptyTail elseClause
    go ((test, body) : rest) = do
        testValue <- evaluate test
        if truthy testValue
            then evalNonEmptyTail body
            else go rest

evalAndTail :: [Expr] -> Eval EvalStep
evalAndTail [] = pure $ StepDone (VBool True)
evalAndTail [expr] = do
    env <- ask
    pure $ StepExpr env expr
evalAndTail (expr : rest) = do
    value <- evaluate expr
    if truthy value
        then evalAndTail rest
        else pure $ StepDone (VBool False)

evalOrTail :: [Expr] -> Eval EvalStep
evalOrTail [] = pure $ StepDone (VBool False)
evalOrTail [expr] = do
    env <- ask
    pure $ StepExpr env expr
evalOrTail (expr : rest) = do
    value <- evaluate expr
    if truthy value
        then pure $ StepDone value
        else evalOrTail rest

evalDoTail :: [(Id, Expr, Expr)] -> (Expr, [Expr]) -> Body -> Eval EvalStep
evalDoTail bindings test body = do
    env <- ask
    initValues <- traverse evalInit bindings
    firstIterationEnv <- liftIO $ bindIterationFrame env initValues
    pure $ StepDo env firstIterationEnv bindings test body
  where
    bindIterationFrame :: Env -> [(Id, Value)] -> IO Env
    bindIterationFrame parentEnv values = do
        iterationEnv <- newChildFrame parentEnv
        for_ values $ \(ident, value) ->
            bindInFrame ident value iterationEnv
        pure iterationEnv

    evalInit :: (Id, Expr, Expr) -> Eval (Id, Value)
    evalInit (ident, initExpr, _) = do
        value <- evaluate initExpr
        pure (ident, value)

evalDoIterationTail ::
    Env ->
    Env ->
    [(Id, Expr, Expr)] ->
    (Expr, [Expr]) ->
    Body ->
    Eval EvalStep
evalDoIterationTail parentEnv _ bindings (testExpr, resultExprs) body = do
    testValue <- evaluate testExpr
    if truthy testValue
        then evalSequenceTail resultExprs
        else do
            _ <- evalBody body
            stepValues <- traverse evalStep bindings
            nextEnv <- liftIO $ bindIterationFrame parentEnv stepValues
            pure $ StepDo parentEnv nextEnv bindings (testExpr, resultExprs) body
  where
    bindIterationFrame :: Env -> [(Id, Value)] -> IO Env
    bindIterationFrame parent values = do
        iterationEnv <- newChildFrame parent
        for_ values $ \(ident, value) ->
            bindInFrame ident value iterationEnv
        pure iterationEnv

    evalStep :: (Id, Expr, Expr) -> Eval (Id, Value)
    evalStep (ident, _, stepExpr) = do
        value <- evaluate stepExpr
        pure (ident, value)

evalBody :: Body -> Eval Value
evalBody body = evalBodyTail body >>= drive

evalBodyTail :: Body -> Eval EvalStep
evalBodyTail (Body defines exprs) = do
    for_ defines $ \(Define ident _) ->
        defineUninitializedVar ident
    evaluatedDefines <- forM defines $ \(Define ident rhs) -> do
        value <- evaluate rhs
        pure (ident, value)
    for_ evaluatedDefines $ uncurry setVar
    evalNonEmptyTail exprs

evalNonEmptyTail :: NonEmpty Expr -> Eval EvalStep
evalNonEmptyTail (expr :| rest) = evalSequenceTail (expr : rest)

evalSequenceTail :: [Expr] -> Eval EvalStep
evalSequenceTail [] = pure $ StepDone VUnspecified
evalSequenceTail [expr] = do
    env <- ask
    pure $ StepExpr env expr
evalSequenceTail (expr : rest) = do
    _ <- evaluate expr
    env <- ask
    pure $ StepSequence env rest

quoteToValue :: SExpr -> Eval Value
quoteToValue sexpr = case sexpr of
    SNum n -> pure $ VNum n
    SBool b -> pure $ VBool b
    SStr s -> pure $ VStr s
    SSym s -> pure $ VSym s
    SNil -> pure VNil
    SPair car cdr -> do
        carValue <- quoteToValue car
        cdrValue <- quoteToValue cdr
        carRef <- liftIO $ newIORef carValue
        cdrRef <- liftIO $ newIORef cdrValue
        pure $ VPair carRef cdrRef

listToValue :: [Value] -> Eval Value
listToValue [] = pure VNil
listToValue (value : rest) = do
    restValue <- listToValue rest
    valueRef <- liftIO $ newIORef value
    restRef <- liftIO $ newIORef restValue
    pure $ VPair valueRef restRef

truthy :: Value -> Bool
truthy (VBool False) = False
truthy _ = True
