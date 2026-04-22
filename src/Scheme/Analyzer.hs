{- | Convert S-expressions into typed AST nodes.
Validates syntax and rejects malformed input with descriptive errors.
-}
module Scheme.Analyzer (
    analyze,
    analyzeToplevel,
    AnalyzeError (AnalyzeError),
) where

import Scheme.AST (
    Body (Body),
    CondClauses (CondClauses, CondElseOnly),
    Define (Define),
    Expr (EAnd, EApp, EBegin, ECond, EDo, EIf, ELambda, ELet, ELetStar, ELetrec, ELit, EOr, EQuote, ESet, EVar),
    Id,
    Literal (LBool, LNil, LNum, LStr),
    Params (Params),
    Toplevel (TDefine, TExpr, TLoad),
    idToText,
    mkId,
 )
import Scheme.SExpr (SExpr (SBool, SNil, SNum, SPair, SStr, SSym))

-- | Analysis error with a descriptive message.
newtype AnalyzeError = AnalyzeError Text
    deriving newtype (Show, Eq)

-- * Public API

-- | Analyze an S-expression as a top-level form.
analyzeToplevel :: SExpr -> Either AnalyzeError Toplevel
analyzeToplevel sexpr = case sexpr of
    SPair (SSym "define") rest -> TDefine <$> analyzeDefine rest
    SPair (SSym "load") rest -> TLoad <$> analyzeLoad rest
    _ -> TExpr <$> analyze sexpr

{- | Analyze an S-expression as an expression.
Ordered to match the spec grammar: Exp ::= Const | Id | (lambda ...) | ...
-}
analyze :: SExpr -> Either AnalyzeError Expr
analyze sexpr = case sexpr of
    -- Const
    SNum n -> Right $ ELit (LNum n)
    SBool b -> Right $ ELit (LBool b)
    SStr s -> Right $ ELit (LStr s)
    SNil -> Right $ ELit LNil
    -- Id
    SSym s -> EVar <$> requireId s
    -- (lambda Arg Body)
    SPair (SSym "lambda") rest -> analyzeLambda rest
    -- (quote S-Exp)
    SPair (SSym "quote") rest -> analyzeQuote rest
    -- (set! Id Exp)
    SPair (SSym "set!") rest -> analyzeSet rest
    -- (let [Id] Bindings Body)
    SPair (SSym "let") rest -> analyzeLet rest
    -- (let* Bindings Body)
    SPair (SSym "let*") rest -> analyzeLetStar rest
    -- (letrec Bindings Body)
    SPair (SSym "letrec") rest -> analyzeLetrec rest
    -- (if Exp Exp [Exp])
    SPair (SSym "if") rest -> analyzeIf rest
    -- (cond ...)
    SPair (SSym "cond") rest -> analyzeCond rest
    -- (and Exp*)
    SPair (SSym "and") rest -> EAnd <$> analyzeList rest
    -- (or Exp*)
    SPair (SSym "or") rest -> EOr <$> analyzeList rest
    -- (begin Exp*)
    SPair (SSym "begin") rest -> EBegin <$> analyzeList rest
    -- (do ((Id Exp Exp)*) (Exp Exp*) Body)
    SPair (SSym "do") rest -> analyzeDo rest
    -- define in expression context is an error
    SPair (SSym "define") rest -> analyzeInternalDefine rest
    -- load is only allowed at top level
    SPair (SSym "load") _ ->
        Left $ AnalyzeError "load: not allowed in expression context (top level only)"
    -- Function application: (Exp Exp*)
    SPair func args -> EApp <$> analyze func <*> analyzeList args

-- * Special form analyzers (ordered to match analyze dispatch)

analyzeLambda :: SExpr -> Either AnalyzeError Expr
analyzeLambda rest = do
    args <- sexpToList rest
    case args of
        (paramSpec : bodyExprs@(_ : _)) -> do
            params <- analyzeParams paramSpec
            body <- analyzeBody bodyExprs
            Right $ ELambda params body
        _ -> Left $ AnalyzeError "lambda: expected parameters and body"

analyzeQuote :: SExpr -> Either AnalyzeError Expr
analyzeQuote rest = do
    args <- sexpToList rest
    case args of
        [x] -> Right $ EQuote x
        _ -> Left $ AnalyzeError "quote: expected exactly one argument"

analyzeSet :: SExpr -> Either AnalyzeError Expr
analyzeSet rest = do
    args <- sexpToList rest
    case args of
        [SSym name, expr] -> ESet <$> requireId name <*> analyze expr
        _ -> Left $ AnalyzeError "set!: expected (set! identifier expression)"

analyzeLet :: SExpr -> Either AnalyzeError Expr
analyzeLet rest = do
    args <- sexpToList rest
    case args of
        -- Named let: (let name ((var val) ...) body...)
        (SSym name : bindingsExpr : bodyExprs@(_ : _)) -> do
            nameId <- requireId name
            bindings <- analyzeBindings bindingsExpr
            checkDistinct "let" (fmap fst bindings)
            body <- analyzeBody bodyExprs
            Right $ ELet (Just nameId) bindings body
        -- Regular let: (let ((var val) ...) body...)
        (bindingsExpr : bodyExprs@(_ : _)) -> do
            bindings <- analyzeBindings bindingsExpr
            checkDistinct "let" (fmap fst bindings)
            body <- analyzeBody bodyExprs
            Right $ ELet Nothing bindings body
        (SSym _ : _) -> Left $ AnalyzeError "let: named let requires bindings and body"
        _ -> Left $ AnalyzeError "let: expected bindings and body"

analyzeLetStar :: SExpr -> Either AnalyzeError Expr
analyzeLetStar rest = do
    args <- sexpToList rest
    case args of
        (bindingsExpr : bodyExprs@(_ : _)) -> do
            bindings <- analyzeBindings bindingsExpr
            body <- analyzeBody bodyExprs
            Right $ ELetStar bindings body
        _ -> Left $ AnalyzeError "let*: expected bindings and body"

analyzeLetrec :: SExpr -> Either AnalyzeError Expr
analyzeLetrec rest = do
    args <- sexpToList rest
    case args of
        (bindingsExpr : bodyExprs@(_ : _)) -> do
            bindings <- analyzeBindings bindingsExpr
            checkDistinct "letrec" (fmap fst bindings)
            body <- analyzeBody bodyExprs
            Right $ ELetrec bindings body
        _ -> Left $ AnalyzeError "letrec: expected bindings and body"

analyzeIf :: SExpr -> Either AnalyzeError Expr
analyzeIf rest = do
    args <- sexpToList rest
    case args of
        [c, t] -> EIf <$> analyze c <*> analyze t <*> pure Nothing
        [c, t, e] -> EIf <$> analyze c <*> analyze t <*> (Just <$> analyze e)
        _ -> Left $ AnalyzeError "if: expected 2 or 3 arguments"

analyzeCond :: SExpr -> Either AnalyzeError Expr
analyzeCond rest = do
    clauses <- sexpToList rest
    (regularClauses, elseClause) <- splitElse clauses
    case nonEmpty regularClauses of
        Just ne -> Right $ ECond (CondClauses ne elseClause)
        Nothing -> case elseClause of
            Just body -> Right $ ECond (CondElseOnly body)
            Nothing -> Left $ AnalyzeError "cond: expected at least one clause"
  where
    splitElse :: [SExpr] -> Either AnalyzeError ([(Expr, NonEmpty Expr)], Maybe (NonEmpty Expr))
    splitElse [] = Right ([], Nothing)
    splitElse [SPair (SSym "else") elseBody] = do
        body <- analyzeNonEmptyList elseBody
        Right ([], Just body)
    splitElse (SPair (SSym "else") _ : _ : _) =
        Left $ AnalyzeError "cond: else clause must be last"
    splitElse (clause : rest') = do
        (test, body) <- analyzeCondClause clause
        (restClauses, mElse) <- splitElse rest'
        Right ((test, body) : restClauses, mElse)

    analyzeCondClause :: SExpr -> Either AnalyzeError (Expr, NonEmpty Expr)
    analyzeCondClause clause = do
        elems <- sexpToList clause
        case elems of
            (test : bodyExprs@(_ : _)) -> do
                testExpr <- analyze test
                bodyNE <- analyzeNonEmptyExprs bodyExprs
                Right (testExpr, bodyNE)
            _ -> Left $ AnalyzeError "cond: clause must have test and at least one body expression"

analyzeDo :: SExpr -> Either AnalyzeError Expr
analyzeDo rest = do
    args <- sexpToList rest
    case args of
        (bindingsExpr : testExpr : bodyExprs) -> do
            bindings <- analyzeDoBindings bindingsExpr
            test <- analyzeDoTest testExpr
            body <- analyzeBody bodyExprs
            Right $ EDo bindings test body
        _ -> Left $ AnalyzeError "do: expected bindings, test, and body"

analyzeInternalDefine :: SExpr -> Either AnalyzeError Expr
analyzeInternalDefine _ =
    Left $ AnalyzeError "define: not allowed in expression context (use at top level or in body)"

-- * Top-level only forms

analyzeDefine :: SExpr -> Either AnalyzeError Define
analyzeDefine rest = do
    args <- sexpToList rest
    case args of
        -- (define x expr)
        [SSym name, expr] -> Define <$> requireId name <*> analyze expr
        -- (define (f args...) body...) => desugar to (define f (lambda args body))
        (SPair (SSym name) paramList : bodyExprs@(_ : _)) -> do
            nameId <- requireId name
            params <- analyzeParamList paramList
            body <- analyzeBody bodyExprs
            Right $ Define nameId (ELambda params body)
        [SPair (SSym _) _] -> Left $ AnalyzeError "define: function body required"
        _ -> Left $ AnalyzeError "define: invalid syntax"

analyzeLoad :: SExpr -> Either AnalyzeError Text
analyzeLoad rest = do
    args <- sexpToList rest
    case args of
        [SStr path] -> Right path
        [_] -> Left $ AnalyzeError "load: expected a string argument"
        _ -> Left $ AnalyzeError "load: expected exactly one argument"

-- * Helpers: SExpr list conversion

-- | Convert an SExpr proper list to a Haskell list.
sexpToList :: SExpr -> Either AnalyzeError [SExpr]
sexpToList SNil = Right []
sexpToList (SPair x rest) = (x :) <$> sexpToList rest
sexpToList _ = Left $ AnalyzeError "Expected a proper list"

-- | Analyze each element of an SExpr list as an expression.
analyzeList :: SExpr -> Either AnalyzeError [Expr]
analyzeList sexpr = sexpToList sexpr >>= traverse analyze

-- | Analyze an SExpr list as a NonEmpty list of expressions.
analyzeNonEmptyList :: SExpr -> Either AnalyzeError (NonEmpty Expr)
analyzeNonEmptyList sexpr = do
    exprs <- analyzeList sexpr
    case nonEmpty exprs of
        Just ne -> Right ne
        Nothing -> Left $ AnalyzeError "Expected at least one expression"

-- | Convert a Haskell list of SExprs to NonEmpty analyzed expressions.
analyzeNonEmptyExprs :: [SExpr] -> Either AnalyzeError (NonEmpty Expr)
analyzeNonEmptyExprs [] = Left $ AnalyzeError "Expected at least one expression"
analyzeNonEmptyExprs (x : xs) = do
    first' <- analyze x
    rest' <- traverse analyze xs
    Right (first' :| rest')

-- * Helpers: parameter and binding parsing

-- | Parse a parameter specification: Id or (Id* [Id . Id])
analyzeParams :: SExpr -> Either AnalyzeError Params
analyzeParams (SSym name) = do
    -- (lambda args body) — single symbol captures all args
    nameId <- requireId name
    Right $ Params [] (Just nameId)
analyzeParams sexpr = analyzeParamList sexpr

-- | Parse a parameter list: (x y) or (x y . rest)
analyzeParamList :: SExpr -> Either AnalyzeError Params
analyzeParamList sexpr = do
    params <- go sexpr []
    let Params fixed restArg = params
    checkDistinct "parameter list" (fixed <> foldMap pure restArg)
    pure params
  where
    go :: SExpr -> [Id] -> Either AnalyzeError Params
    go SNil acc = Right $ Params (reverse acc) Nothing
    go (SSym name) acc = do
        nameId <- requireId name
        Right $ Params (reverse acc) (Just nameId)
    go (SPair (SSym name) rest') acc = do
        nameId <- requireId name
        go rest' (nameId : acc)
    go _ _ = Left $ AnalyzeError "Invalid parameter list"

-- | Parse let-style bindings: ((var expr) ...)
analyzeBindings :: SExpr -> Either AnalyzeError [(Id, Expr)]
analyzeBindings sexpr = do
    bindingList <- sexpToList sexpr
    traverse analyzeBinding bindingList
  where
    analyzeBinding :: SExpr -> Either AnalyzeError (Id, Expr)
    analyzeBinding (SPair (SSym name) (SPair expr SNil)) = do
        nameId <- requireId name
        val <- analyze expr
        Right (nameId, val)
    analyzeBinding _ = Left $ AnalyzeError "Invalid binding: expected (identifier expression)"

-- | Parse do-loop bindings: ((var init step) ...)
analyzeDoBindings :: SExpr -> Either AnalyzeError [(Id, Expr, Expr)]
analyzeDoBindings sexpr = do
    bindingList <- sexpToList sexpr
    bindings <- traverse analyzeDoBinding bindingList
    checkDistinct "do" [i | (i, _, _) <- bindings]
    pure bindings
  where
    analyzeDoBinding :: SExpr -> Either AnalyzeError (Id, Expr, Expr)
    analyzeDoBinding binding = do
        elems <- sexpToList binding
        case elems of
            [SSym name, initExpr, stepExpr] -> do
                nameId <- requireId name
                init' <- analyze initExpr
                step <- analyze stepExpr
                Right (nameId, init', step)
            _ -> Left $ AnalyzeError "do: binding must be (variable init step)"

-- | Parse do-loop test clause: (test result*)
analyzeDoTest :: SExpr -> Either AnalyzeError (Expr, [Expr])
analyzeDoTest sexpr = do
    elems <- sexpToList sexpr
    case elems of
        (test : results) -> do
            testExpr <- analyze test
            resultExprs <- traverse analyze results
            Right (testExpr, resultExprs)
        [] -> Left $ AnalyzeError "do: test clause must have at least a test expression"

-- * Helpers: body parsing

-- | Parse a body: Define* Exp+
analyzeBody :: [SExpr] -> Either AnalyzeError Body
analyzeBody = collect []
  where
    collect :: [Define] -> [SExpr] -> Either AnalyzeError Body
    collect acc (SPair (SSym "define") rest : xs) = do
        d <- analyzeDefine rest
        collect (d : acc) xs
    collect acc xs = do
        exprs <- traverse analyze xs
        case nonEmpty exprs of
            Just ne -> Right $ Body (reverse acc) ne
            Nothing -> Left $ AnalyzeError "Body must contain at least one expression"

-- * Helpers: Id construction

-- | Construct an Id from Text, failing if empty.
requireId :: Text -> Either AnalyzeError Id
requireId name = case mkId name of
    Just i -> Right i
    Nothing -> Left $ AnalyzeError "Empty identifier"

-- | Fail if a list of Ids contains any duplicate.
checkDistinct :: Text -> [Id] -> Either AnalyzeError ()
checkDistinct ctx = go []
  where
    go _ [] = Right ()
    go seen (x : xs)
        | x `elem` seen =
            Left $ AnalyzeError (ctx <> ": duplicate identifier: " <> idToText x)
        | otherwise = go (x : seen) xs
