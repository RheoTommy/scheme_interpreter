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
import Control.Monad.Cont (ContT (ContT), callCC, runContT)
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
    Value (
        VBool,
        VBuiltin,
        VClosure,
        VContinuation,
        VMacro,
        VNil,
        VNum,
        VPair,
        VStr,
        VSym,
        VUnspecified
    ),
    bindInFrame,
    bindUninitializedInFrame,
    defineVar,
    lookupVar,
    newChildFrame,
    setVar,
    showValueKind,
 )
import Scheme.SExpr (SExpr (SBool, SNil, SNum, SPair, SStr, SSym))

type Binding = (Id, Expr)
type EvaluatedBinding = (Id, Value)
type DoBinding = (Id, Expr, Expr)
type DoTest = (Expr, [Expr])
type CondClause = (Expr, NonEmpty Expr)

type EvalC a = ContT Value (ExceptT EvalError (ReaderT Env IO)) a

-- | Evaluate a single expression in the 'Eval' monad.
evaluate :: Expr -> Eval Value
evaluate expr = do
    env <- ask
    runEvalC $ evalExpr env expr

-- | Evaluate a top-level definition in the current environment.
evaluateDefine :: Define -> Eval Value
evaluateDefine (Define ident rhs) = do
    env <- ask
    runEvalC $ do
        value <- evalExpr env rhs
        liftEval $ local (const env) (defineVar ident value)
        pure VUnspecified

runEvalC :: EvalC Value -> Eval Value
runEvalC computation =
    runContT computation pure

liftEval :: Eval a -> EvalC a
liftEval = lift

evalExpr :: Env -> Expr -> EvalC Value
evalExpr env expr = do
    liftIO yield
    case expr of
        ELit lit -> pure $ literalToValue lit
        EVar ident -> liftEval $ local (const env) (lookupVar ident)
        ELambda params body -> pure $ VClosure params body env
        EApp fExpr argExprs -> do
            procedure <- evalExpr env fExpr
            args <- traverse (evalExpr env) argExprs
            applyProcedure procedure args
        EQuote sexpr -> liftEval $ quoteToValue sexpr
        ESet ident rhs -> do
            value <- evalExpr env rhs
            liftEval $ local (const env) (setVar ident value)
            pure VUnspecified
        ELet name bindings body ->
            evalLet env name bindings body
        ELetStar bindings body ->
            evalLetStar env bindings body
        ELetrec bindings body ->
            evalLetrec env bindings body
        EIf cond thenExpr elseExpr -> do
            condValue <- evalExpr env cond
            if truthy condValue
                then evalExpr env thenExpr
                else maybe (pure VUnspecified) (evalExpr env) elseExpr
        ECond clauses ->
            evalCond env clauses
        EAnd exprs ->
            evalAnd env exprs
        EOr exprs ->
            evalOr env exprs
        EBegin exprs ->
            evalSequence env exprs
        EDo bindings test body ->
            evalDo env bindings test body

literalToValue :: Literal -> Value
literalToValue lit = case lit of
    LNum n -> VNum n
    LBool b -> VBool b
    LStr s -> VStr s
    LNil -> VNil

applyProcedure :: Value -> [Value] -> EvalC Value
applyProcedure (VBuiltin name f) args
    | isCallCCBuiltin name = applyCallCC name args
    | otherwise = liftEval $ f args
applyProcedure (VClosure params body closureEnv) args = do
    callEnv <- liftIO $ newChildFrame closureEnv
    liftEval $ bindParams params args callEnv
    evalBody callEnv body
applyProcedure (VContinuation resume) args = case args of
    [arg] -> ContT $ const (resume arg)
    _ -> liftEval $ throwArityError ("continuation: expected exactly 1 argument, got " <> show (length args))
applyProcedure value _ =
    liftEval . throwError $ NotAProcedure (showValueKind value)

applyCallCC :: Text -> [Value] -> EvalC Value
applyCallCC name args = case args of
    [procedure] ->
        callCC $ \kont ->
            applyProcedure procedure [VContinuation (runEvalC . kont)]
    _ -> liftEval $ throwArityError (name <> ": expected exactly 1 argument, got " <> show (length args))

isCallCCBuiltin :: Text -> Bool
isCallCCBuiltin name =
    name == "call/cc" || name == "call-with-current-continuation"

-- | Apply a macro transformer to already-quoted syntax arguments.
applyMacro :: Value -> [Value] -> Eval Value
applyMacro (VMacro params body closureEnv) args = do
    callEnv <- liftIO $ newChildFrame closureEnv
    bindParams params args callEnv
    runEvalC $ evalBody callEnv body
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

evalBody :: Env -> Body -> EvalC Value
evalBody env (Body defines exprs) = do
    for_ defines $ \(Define ident _) ->
        liftIO $ bindUninitializedInFrame ident env
    values <- traverse (evalBodyDefine env) defines
    liftEval $ setBindings env values
    evalSequence env (nonEmptyToList exprs)

evalBodyDefine :: Env -> Define -> EvalC EvaluatedBinding
evalBodyDefine env (Define ident rhs) = do
    value <- evalExpr env rhs
    pure (ident, value)

evalSequence :: Env -> [Expr] -> EvalC Value
evalSequence env exprs = case exprs of
    [] -> pure VUnspecified
    [expr] -> evalExpr env expr
    expr : rest -> do
        _ <- evalExpr env expr
        evalSequence env rest

evalLet :: Env -> Maybe Id -> [Binding] -> Body -> EvalC Value
evalLet env Nothing bindings body = do
    values <- traverse (evalBinding env) bindings
    child <- liftIO $ newChildFrame env
    liftEval $ bindValues child values
    evalBody child body
evalLet env (Just name) bindings body = do
    child <- liftIO $ newChildFrame env
    let params = Params (fst <$> bindings) Nothing
        closure = VClosure params body child
    liftIO $ bindInFrame name closure child
    args <- traverse (evalExpr env . snd) bindings
    applyProcedure closure args

evalBinding :: Env -> Binding -> EvalC EvaluatedBinding
evalBinding env (ident, rhs) = do
    value <- evalExpr env rhs
    pure (ident, value)

evalLetStar :: Env -> [Binding] -> Body -> EvalC Value
evalLetStar env bindings body = do
    base <- liftIO $ newChildFrame env
    go base bindings
  where
    go :: Env -> [Binding] -> EvalC Value
    go currentEnv [] =
        evalBody currentEnv body
    go currentEnv ((ident, rhs) : rest) = do
        value <- evalExpr currentEnv rhs
        child <- liftIO $ newChildFrame currentEnv
        liftIO $ bindInFrame ident value child
        go child rest

evalLetrec :: Env -> [Binding] -> Body -> EvalC Value
evalLetrec env bindings body = do
    child <- liftIO $ newChildFrame env
    for_ bindings $ \(ident, _) ->
        liftIO $ bindUninitializedInFrame ident child
    values <- traverse (evalBinding child) bindings
    liftEval $ setBindings child values
    evalBody child body

evalCond :: Env -> CondClauses -> EvalC Value
evalCond env clauses = case clauses of
    CondElseOnly body ->
        evalSequence env (nonEmptyToList body)
    CondClauses (firstClause :| restClauses) elseClause ->
        evalCondClauses env (firstClause : restClauses) elseClause

evalCondClauses ::
    Env ->
    [CondClause] ->
    Maybe (NonEmpty Expr) ->
    EvalC Value
evalCondClauses env clauses elseClause = case clauses of
    [] -> maybe (pure VUnspecified) (evalSequence env . nonEmptyToList) elseClause
    (test, body) : rest -> do
        testValue <- evalExpr env test
        if truthy testValue
            then evalSequence env (nonEmptyToList body)
            else evalCondClauses env rest elseClause

evalAnd :: Env -> [Expr] -> EvalC Value
evalAnd env exprs = case exprs of
    [] -> pure $ VBool True
    [expr] -> evalExpr env expr
    expr : rest -> do
        value <- evalExpr env expr
        if truthy value
            then evalAnd env rest
            else pure $ VBool False

evalOr :: Env -> [Expr] -> EvalC Value
evalOr env exprs = case exprs of
    [] -> pure $ VBool False
    [expr] -> evalExpr env expr
    expr : rest -> do
        value <- evalExpr env expr
        if truthy value
            then pure value
            else evalOr env rest

evalDo :: Env -> [DoBinding] -> DoTest -> Body -> EvalC Value
evalDo parentEnv bindings test body = do
    initialValues <- traverse (evalDoInitializer parentEnv) bindings
    evalDoIteration parentEnv bindings test body initialValues

evalDoInitializer :: Env -> DoBinding -> EvalC EvaluatedBinding
evalDoInitializer parentEnv (ident, initExpr, _) = do
    value <- evalExpr parentEnv initExpr
    pure (ident, value)

evalDoIteration :: Env -> [DoBinding] -> DoTest -> Body -> [EvaluatedBinding] -> EvalC Value
evalDoIteration parentEnv bindings test body values = do
    iterationEnv <- liftIO $ bindIterationFrame parentEnv values
    testValue <- evalExpr iterationEnv (fst test)
    if truthy testValue
        then evalSequence iterationEnv (snd test)
        else do
            _ <- evalBody iterationEnv body
            nextValues <- traverse (evalDoStep iterationEnv) bindings
            evalDoIteration parentEnv bindings test body nextValues

evalDoStep :: Env -> DoBinding -> EvalC EvaluatedBinding
evalDoStep iterationEnv (ident, _, stepExpr) = do
    value <- evalExpr iterationEnv stepExpr
    pure (ident, value)

bindIterationFrame :: Env -> [EvaluatedBinding] -> IO Env
bindIterationFrame parentEnv values = do
    iterationEnv <- newChildFrame parentEnv
    for_ values $ \(ident, value) ->
        bindInFrame ident value iterationEnv
    pure iterationEnv

bindValues :: Env -> [EvaluatedBinding] -> Eval ()
bindValues env values =
    for_ values $ \(ident, value) ->
        liftIO $ bindInFrame ident value env

setBindings :: Env -> [EvaluatedBinding] -> Eval ()
setBindings env values =
    local (const env) . for_ values $ uncurry setVar

nonEmptyToList :: NonEmpty a -> [a]
nonEmptyToList (value :| values) = value : values

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
