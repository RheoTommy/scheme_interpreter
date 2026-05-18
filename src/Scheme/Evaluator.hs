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

data EvalState
    = Done Value
    | Continue Kont Value
    | EvalExpr Env Expr Kont
    | EvalBody Env Body Kont
    | EvalSequence Env [Expr] Kont
    | Apply Value [Value] Kont
    | EvalLetStar Env [Binding] Body Kont
    | EvalDoIteration Env Env [DoBinding] DoTest Body Kont

data Kont
    = KDone
    | KDefine Env Id Kont
    | KSet Env Id Kont
    | KIf Env Expr (Maybe Expr) Kont
    | KSequence Env [Expr] Kont
    | KAppFun Env [Expr] Kont
    | KAppArg Env Value [Value] [Expr] Kont
    | KBodyDefine Env Id [Define] [EvaluatedBinding] (NonEmpty Expr) Kont
    | KLet Env Id [Binding] [EvaluatedBinding] Body Kont
    | KLetStarBind Env Id [Binding] Body Kont
    | KLetrec Env Id [Binding] [EvaluatedBinding] Body Kont
    | KAnd Env [Expr] Kont
    | KOr Env [Expr] Kont
    | KCond Env (NonEmpty Expr) [CondClause] (Maybe (NonEmpty Expr)) Kont
    | KDoInit Env [DoBinding] Id [DoBinding] [EvaluatedBinding] DoTest Body Kont
    | KDoTest Env Env [DoBinding] DoTest Body Kont
    | KDoBody Env Env [DoBinding] DoTest Body Kont
    | KDoStep Env Env [DoBinding] Id [DoBinding] [EvaluatedBinding] DoTest Body Kont

-- | Evaluate a single expression in the 'Eval' monad.
evaluate :: Expr -> Eval Value
evaluate expr = do
    env <- ask
    drive (EvalExpr env expr KDone)

-- | Evaluate a top-level definition in the current environment.
evaluateDefine :: Define -> Eval Value
evaluateDefine (Define ident rhs) = do
    env <- ask
    drive (EvalExpr env rhs (KDefine env ident KDone))

drive :: EvalState -> Eval Value
drive machine = do
    liftIO yield
    case machine of
        Done value -> pure value
        _ -> step machine >>= drive

step :: EvalState -> Eval EvalState
step machine = case machine of
    Done{} -> pure machine
    Continue kont value -> continue kont value
    EvalExpr env expr kont -> evalExpr env expr kont
    EvalBody env body kont -> evalBodyState env body kont
    EvalSequence env exprs kont -> evalSequenceState env exprs kont
    Apply procedure args kont -> applyProcedure procedure args kont
    EvalLetStar env bindings body kont -> evalLetStarState env bindings body kont
    EvalDoIteration parentEnv iterationEnv bindings test body kont ->
        pure $ EvalExpr iterationEnv (fst test) (KDoTest parentEnv iterationEnv bindings test body kont)

evalExpr :: Env -> Expr -> Kont -> Eval EvalState
evalExpr env expr kont = case expr of
    ELit lit -> pure $ Continue kont (literalToValue lit)
    EVar ident -> Continue kont <$> local (const env) (lookupVar ident)
    ELambda params body -> pure $ Continue kont (VClosure params body env)
    EApp fExpr argExprs -> pure $ EvalExpr env fExpr (KAppFun env argExprs kont)
    EQuote sexpr -> Continue kont <$> quoteToValue sexpr
    ESet ident rhs -> pure $ EvalExpr env rhs (KSet env ident kont)
    ELet name bindings body -> evalLetState env name bindings body kont
    ELetStar bindings body -> pure $ EvalLetStar env bindings body kont
    ELetrec bindings body -> evalLetrecState env bindings body kont
    EIf cond thenExpr elseExpr -> pure $ EvalExpr env cond (KIf env thenExpr elseExpr kont)
    ECond clauses -> pure $ evalCondState env clauses kont
    EAnd exprs -> pure $ evalAndState env exprs kont
    EOr exprs -> pure $ evalOrState env exprs kont
    EBegin exprs -> pure $ EvalSequence env exprs kont
    EDo bindings test body -> evalDoState env bindings test body kont

continue :: Kont -> Value -> Eval EvalState
continue kont value = case kont of
    KDone -> pure $ Done value
    KDefine env ident next -> do
        local (const env) (defineVar ident value)
        pure $ Continue next VUnspecified
    KSet env ident next -> do
        local (const env) (setVar ident value)
        pure $ Continue next VUnspecified
    KIf env thenExpr elseExpr next ->
        if truthy value
            then pure $ EvalExpr env thenExpr next
            else pure $ maybe (Continue next VUnspecified) (\expr -> EvalExpr env expr next) elseExpr
    KSequence env rest next ->
        pure $ EvalSequence env rest next
    KAppFun env argExprs next ->
        pure $ evalApplicationArgs env value argExprs next
    KAppArg env procedure revArgs rest next -> do
        let revArgs' = value : revArgs
        pure $ case rest of
            [] -> Apply procedure (reverse revArgs') next
            argExpr : remaining -> EvalExpr env argExpr (KAppArg env procedure revArgs' remaining next)
    KBodyDefine env ident rest acc exprs next ->
        continueBodyDefine env ident rest acc exprs next value
    KLet env ident rest acc body next ->
        continueLet env ident rest acc body next value
    KLetStarBind env ident rest body next ->
        continueLetStar env ident rest body next value
    KLetrec env ident rest acc body next ->
        continueLetrec env ident rest acc body next value
    KAnd env rest next ->
        if truthy value
            then pure $ evalAndState env rest next
            else pure $ Continue next (VBool False)
    KOr env rest next ->
        if truthy value
            then pure $ Continue next value
            else pure $ evalOrState env rest next
    KCond env body rest elseClause next ->
        if truthy value
            then pure $ EvalSequence env (nonEmptyToList body) next
            else pure $ evalCondClauses env rest elseClause next
    KDoInit parentEnv bindings ident rest acc test body next ->
        continueDoInit parentEnv bindings ident rest acc test body next value
    KDoTest parentEnv iterationEnv bindings test body next ->
        if truthy value
            then pure $ EvalSequence iterationEnv (snd test) next
            else pure $ EvalBody iterationEnv body (KDoBody parentEnv iterationEnv bindings test body next)
    KDoBody parentEnv iterationEnv bindings test body next ->
        evalDoStepsState parentEnv iterationEnv bindings test body next
    KDoStep parentEnv iterationEnv bindings ident rest acc test body next ->
        continueDoStep parentEnv iterationEnv bindings ident rest acc test body next value

literalToValue :: Literal -> Value
literalToValue lit = case lit of
    LNum n -> VNum n
    LBool b -> VBool b
    LStr s -> VStr s
    LNil -> VNil

applyProcedure :: Value -> [Value] -> Kont -> Eval EvalState
applyProcedure (VBuiltin name f) args kont
    | isCallCCBuiltin name = applyCallCC name args kont
    | otherwise = Continue kont <$> f args
applyProcedure (VClosure params body closureEnv) args kont = do
    callEnv <- liftIO $ newChildFrame closureEnv
    bindParams params args callEnv
    pure $ EvalBody callEnv body kont
applyProcedure (VContinuation resume) args _ = case args of
    [arg] -> Done <$> resume arg
    _ -> throwArityError ("continuation: expected exactly 1 argument, got " <> show (length args))
applyProcedure value _ _ =
    throwError $ NotAProcedure (showValueKind value)

applyCallCC :: Text -> [Value] -> Kont -> Eval EvalState
applyCallCC name args kont = case args of
    [procedure] ->
        pure $
            Apply
                procedure
                [VContinuation (drive . Continue kont)]
                kont
    _ -> throwArityError (name <> ": expected exactly 1 argument, got " <> show (length args))

isCallCCBuiltin :: Text -> Bool
isCallCCBuiltin name =
    name == "call/cc" || name == "call-with-current-continuation"

-- | Apply a macro transformer to already-quoted syntax arguments.
applyMacro :: Value -> [Value] -> Eval Value
applyMacro (VMacro params body closureEnv) args = do
    callEnv <- liftIO $ newChildFrame closureEnv
    bindParams params args callEnv
    drive (EvalBody callEnv body KDone)
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

evalBodyState :: Env -> Body -> Kont -> Eval EvalState
evalBodyState env (Body defines exprs) kont = do
    for_ defines $ \(Define ident _) ->
        liftIO $ bindUninitializedInFrame ident env
    pure $ case defines of
        [] -> EvalSequence env (nonEmptyToList exprs) kont
        Define ident rhs : rest -> EvalExpr env rhs (KBodyDefine env ident rest [] exprs kont)

continueBodyDefine ::
    Env ->
    Id ->
    [Define] ->
    [EvaluatedBinding] ->
    NonEmpty Expr ->
    Kont ->
    Value ->
    Eval EvalState
continueBodyDefine env ident rest acc exprs kont value =
    let acc' = (ident, value) : acc
     in case rest of
            [] -> do
                setBindings env (reverse acc')
                pure $ EvalSequence env (nonEmptyToList exprs) kont
            Define nextIdent rhs : remaining ->
                pure $ EvalExpr env rhs (KBodyDefine env nextIdent remaining acc' exprs kont)

evalSequenceState :: Env -> [Expr] -> Kont -> Eval EvalState
evalSequenceState env exprs kont =
    pure $ case exprs of
        [] -> Continue kont VUnspecified
        [expr] -> EvalExpr env expr kont
        expr : rest -> EvalExpr env expr (KSequence env rest kont)

evalApplicationArgs :: Env -> Value -> [Expr] -> Kont -> EvalState
evalApplicationArgs env procedure argExprs kont = case argExprs of
    [] -> Apply procedure [] kont
    argExpr : rest -> EvalExpr env argExpr (KAppArg env procedure [] rest kont)

evalLetState :: Env -> Maybe Id -> [Binding] -> Body -> Kont -> Eval EvalState
evalLetState env Nothing bindings body kont = case bindings of
    [] -> do
        child <- liftIO $ newChildFrame env
        pure $ EvalBody child body kont
    (ident, rhs) : rest ->
        pure $ EvalExpr env rhs (KLet env ident rest [] body kont)
evalLetState env (Just name) bindings body kont = do
    child <- liftIO $ newChildFrame env
    let params = Params (fst <$> bindings) Nothing
        closure = VClosure params body child
    liftIO $ bindInFrame name closure child
    pure $ evalApplicationArgs env closure (snd <$> bindings) kont

continueLet ::
    Env ->
    Id ->
    [Binding] ->
    [EvaluatedBinding] ->
    Body ->
    Kont ->
    Value ->
    Eval EvalState
continueLet env ident rest acc body kont value =
    let acc' = (ident, value) : acc
     in case rest of
            [] -> do
                child <- liftIO $ newChildFrame env
                bindValues child (reverse acc')
                pure $ EvalBody child body kont
            (nextIdent, rhs) : remaining ->
                pure $ EvalExpr env rhs (KLet env nextIdent remaining acc' body kont)

evalLetStarState :: Env -> [Binding] -> Body -> Kont -> Eval EvalState
evalLetStarState env bindings body kont = do
    base <- liftIO $ newChildFrame env
    pure $ case bindings of
        [] -> EvalBody base body kont
        (ident, rhs) : rest -> EvalExpr base rhs (KLetStarBind base ident rest body kont)

continueLetStar :: Env -> Id -> [Binding] -> Body -> Kont -> Value -> Eval EvalState
continueLetStar env ident rest body kont value = do
    child <- liftIO $ newChildFrame env
    liftIO $ bindInFrame ident value child
    pure $ case rest of
        [] -> EvalBody child body kont
        (nextIdent, rhs) : remaining -> EvalExpr child rhs (KLetStarBind child nextIdent remaining body kont)

evalLetrecState :: Env -> [Binding] -> Body -> Kont -> Eval EvalState
evalLetrecState env bindings body kont = do
    child <- liftIO $ newChildFrame env
    for_ bindings $ \(ident, _) ->
        liftIO $ bindUninitializedInFrame ident child
    pure $ case bindings of
        [] -> EvalBody child body kont
        (ident, rhs) : rest -> EvalExpr child rhs (KLetrec child ident rest [] body kont)

continueLetrec ::
    Env ->
    Id ->
    [Binding] ->
    [EvaluatedBinding] ->
    Body ->
    Kont ->
    Value ->
    Eval EvalState
continueLetrec env ident rest acc body kont value =
    let acc' = (ident, value) : acc
     in case rest of
            [] -> do
                setBindings env (reverse acc')
                pure $ EvalBody env body kont
            (nextIdent, rhs) : remaining ->
                pure $ EvalExpr env rhs (KLetrec env nextIdent remaining acc' body kont)

evalCondState :: Env -> CondClauses -> Kont -> EvalState
evalCondState env clauses kont = case clauses of
    CondElseOnly body -> EvalSequence env (nonEmptyToList body) kont
    CondClauses (firstClause :| restClauses) elseClause ->
        evalCondClauses env (firstClause : restClauses) elseClause kont

evalCondClauses ::
    Env ->
    [CondClause] ->
    Maybe (NonEmpty Expr) ->
    Kont ->
    EvalState
evalCondClauses env clauses elseClause kont = case clauses of
    [] -> maybe (Continue kont VUnspecified) (\body -> EvalSequence env (nonEmptyToList body) kont) elseClause
    (test, body) : rest -> EvalExpr env test (KCond env body rest elseClause kont)

evalAndState :: Env -> [Expr] -> Kont -> EvalState
evalAndState env exprs kont = case exprs of
    [] -> Continue kont (VBool True)
    [expr] -> EvalExpr env expr kont
    expr : rest -> EvalExpr env expr (KAnd env rest kont)

evalOrState :: Env -> [Expr] -> Kont -> EvalState
evalOrState env exprs kont = case exprs of
    [] -> Continue kont (VBool False)
    [expr] -> EvalExpr env expr kont
    expr : rest -> EvalExpr env expr (KOr env rest kont)

evalDoState :: Env -> [DoBinding] -> DoTest -> Body -> Kont -> Eval EvalState
evalDoState parentEnv bindings test body kont = case bindings of
    [] -> do
        iterationEnv <- liftIO $ bindIterationFrame parentEnv []
        pure $ EvalDoIteration parentEnv iterationEnv bindings test body kont
    (ident, initExpr, _) : rest ->
        pure $ EvalExpr parentEnv initExpr (KDoInit parentEnv bindings ident rest [] test body kont)

continueDoInit ::
    Env ->
    [DoBinding] ->
    Id ->
    [DoBinding] ->
    [EvaluatedBinding] ->
    DoTest ->
    Body ->
    Kont ->
    Value ->
    Eval EvalState
continueDoInit parentEnv bindings ident rest acc test body kont value =
    let acc' = (ident, value) : acc
     in case rest of
            [] -> do
                iterationEnv <- liftIO $ bindIterationFrame parentEnv (reverse acc')
                pure $ EvalDoIteration parentEnv iterationEnv bindings test body kont
            (nextIdent, initExpr, _) : remaining ->
                pure $ EvalExpr parentEnv initExpr (KDoInit parentEnv bindings nextIdent remaining acc' test body kont)

evalDoStepsState :: Env -> Env -> [DoBinding] -> DoTest -> Body -> Kont -> Eval EvalState
evalDoStepsState parentEnv iterationEnv bindings test body kont = case bindings of
    [] -> do
        nextEnv <- liftIO $ bindIterationFrame parentEnv []
        pure $ EvalDoIteration parentEnv nextEnv bindings test body kont
    (ident, _, stepExpr) : rest ->
        pure $ EvalExpr iterationEnv stepExpr (KDoStep parentEnv iterationEnv bindings ident rest [] test body kont)

continueDoStep ::
    Env ->
    Env ->
    [DoBinding] ->
    Id ->
    [DoBinding] ->
    [EvaluatedBinding] ->
    DoTest ->
    Body ->
    Kont ->
    Value ->
    Eval EvalState
continueDoStep parentEnv iterationEnv bindings ident rest acc test body kont value =
    let acc' = (ident, value) : acc
     in case rest of
            [] -> do
                nextEnv <- liftIO $ bindIterationFrame parentEnv (reverse acc')
                pure $ EvalDoIteration parentEnv nextEnv bindings test body kont
            (nextIdent, _, stepExpr) : remaining ->
                pure $ EvalExpr iterationEnv stepExpr (KDoStep parentEnv iterationEnv bindings nextIdent remaining acc' test body kont)

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
