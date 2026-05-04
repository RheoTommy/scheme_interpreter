{- | Evaluator: 'Expr' -> 'Value' in the 'Eval' monad.

Responsibility is deliberately limited to @Expr -> Value@: parsing and
analysis live upstream in 'Scheme.Parser' and 'Scheme.Analyzer', and
the end-to-end @Text -> Value@ driver lives in 'Scheme.Interpreter'.
-}
module Scheme.Evaluator (
    evaluate,
    evaluateDefine,
) where

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
    Value (VBool, VBuiltin, VClosure, VNil, VNum, VPair, VStr, VSym, VUnspecified),
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

-- | Evaluate a single expression in the 'Eval' monad.
evaluate :: Expr -> Eval Value
evaluate expr = case expr of
    ELit lit -> pure (literalToValue lit)
    EVar ident -> lookupVar ident
    ELambda params body -> VClosure params body <$> ask
    EApp fExpr argExprs -> do
        -- Function and arguments are evaluated left-to-right, following
        -- 'traverse's sequencing in 'Eval'.
        f <- evaluate fExpr
        args <- traverse evaluate argExprs
        apply f args
    EQuote sexpr -> quoteToValue sexpr
    ESet ident rhs -> do
        value <- evaluate rhs
        setVar ident value
        pure VUnspecified
    ELet name bindings body -> evalLet name bindings body
    ELetStar bindings body -> evalLetStar bindings body
    ELetrec bindings body -> evalLetrec bindings body
    EIf cond thenExpr elseExpr -> evalIf cond thenExpr elseExpr
    ECond clauses -> evalCond clauses
    EAnd exprs -> evalAnd exprs
    EOr exprs -> evalOr exprs
    EBegin exprs -> evalSequence exprs
    EDo bindings test body -> evalDo bindings test body

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

-- | Apply a value to a list of argument values (function call).
apply :: Value -> [Value] -> Eval Value
apply (VBuiltin _ f) args = f args
apply (VClosure params body closureEnv) args = do
    callEnv <- liftIO $ newChildFrame closureEnv
    bindParams params args callEnv
    local (const callEnv) (evalBody body)
apply v _ = throwError $ NotAProcedure (showValueKind v)

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

evalLet :: Maybe Id -> [(Id, Expr)] -> Body -> Eval Value
evalLet Nothing bindings body = do
    env <- ask
    evaluatedBindings <- traverse evalBinding bindings
    child <- liftIO $ newChildFrame env
    for_ evaluatedBindings $ \(ident, value) ->
        liftIO $ bindInFrame ident value child
    local (const child) (evalBody body)
evalLet (Just name) bindings body = do
    env <- ask
    child <- liftIO $ newChildFrame env
    let params = Params (fmap fst bindings) Nothing
        closure = VClosure params body child
    liftIO $ bindInFrame name closure child
    args <- traverse (evaluate . snd) bindings
    apply closure args

evalBinding :: (Id, Expr) -> Eval (Id, Value)
evalBinding (ident, rhs) = do
    value <- evaluate rhs
    pure (ident, value)

evalLetStar :: [(Id, Expr)] -> Body -> Eval Value
evalLetStar bindings body = do
    env <- ask
    base <- liftIO $ newChildFrame env
    go base bindings
  where
    go :: Env -> [(Id, Expr)] -> Eval Value
    go env [] = local (const env) (evalBody body)
    go env ((ident, rhs) : rest) = do
        value <- local (const env) (evaluate rhs)
        child <- liftIO $ newChildFrame env
        liftIO $ bindInFrame ident value child
        go child rest

evalLetrec :: [(Id, Expr)] -> Body -> Eval Value
evalLetrec bindings body = do
    env <- ask
    child <- liftIO $ newChildFrame env
    for_ bindings $ \(ident, _) ->
        liftIO $ bindUninitializedInFrame ident child
    local (const child) $ do
        evaluatedBindings <- traverse evalBinding bindings
        for_ evaluatedBindings $ uncurry setVar
        evalBody body

evalIf :: Expr -> Expr -> Maybe Expr -> Eval Value
evalIf cond thenExpr elseExpr = do
    condValue <- evaluate cond
    if truthy condValue
        then evaluate thenExpr
        else maybe (pure VUnspecified) evaluate elseExpr

evalCond :: CondClauses -> Eval Value
evalCond (CondElseOnly body) = evalNonEmpty body
evalCond (CondClauses (firstClause :| restClauses) elseClause) =
    go (firstClause : restClauses)
  where
    go :: [(Expr, NonEmpty Expr)] -> Eval Value
    go [] = maybe (pure VUnspecified) evalNonEmpty elseClause
    go ((test, body) : rest) = do
        testValue <- evaluate test
        if truthy testValue
            then evalNonEmpty body
            else go rest

evalAnd :: [Expr] -> Eval Value
evalAnd [] = pure $ VBool True
evalAnd [expr] = evaluate expr
evalAnd (expr : rest) = do
    value <- evaluate expr
    if truthy value
        then evalAnd rest
        else pure $ VBool False

evalOr :: [Expr] -> Eval Value
evalOr [] = pure $ VBool False
evalOr (expr : rest) = do
    value <- evaluate expr
    if truthy value
        then pure value
        else evalOr rest

evalDo :: [(Id, Expr, Expr)] -> (Expr, [Expr]) -> Body -> Eval Value
evalDo bindings (testExpr, resultExprs) body = do
    env <- ask
    initValues <- traverse evalInit bindings
    firstIterationEnv <- liftIO $ bindIterationFrame env initValues
    loop env firstIterationEnv
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

    evalStep :: (Id, Expr, Expr) -> Eval (Id, Value)
    evalStep (ident, _, stepExpr) = do
        value <- evaluate stepExpr
        pure (ident, value)

    loop :: Env -> Env -> Eval Value
    loop parentEnv iterationEnv = local (const iterationEnv) $ do
        testValue <- evaluate testExpr
        if truthy testValue
            then evalSequence resultExprs
            else do
                _ <- evalBody body
                stepValues <- traverse evalStep bindings
                nextEnv <- liftIO $ bindIterationFrame parentEnv stepValues
                loop parentEnv nextEnv

evalBody :: Body -> Eval Value
evalBody (Body defines exprs) = do
    for_ defines $ \(Define ident _) ->
        defineUninitializedVar ident
    evaluatedDefines <- forM defines $ \(Define ident rhs) -> do
        value <- evaluate rhs
        pure (ident, value)
    for_ evaluatedDefines $ uncurry setVar
    evalNonEmpty exprs

evalNonEmpty :: NonEmpty Expr -> Eval Value
evalNonEmpty (expr :| rest) = evalSequence (expr : rest)

evalSequence :: [Expr] -> Eval Value
evalSequence [] = pure VUnspecified
evalSequence [expr] = evaluate expr
evalSequence (expr : rest) = evaluate expr *> evalSequence rest

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
