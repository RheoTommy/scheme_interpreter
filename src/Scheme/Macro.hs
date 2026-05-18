{- | Common-Lisp-style macro expansion.

Macros are expanded at the 'SExpr' layer before analysis. Macro
transformers are stored in the ordinary top-level environment as 'VMacro'
values so that REPL-style repeated calls keep macro definitions alive.
-}
module Scheme.Macro (
    ExpandedToplevel (ExpandedExpression, ExpandedMacroDefinition),
    expandToplevel,
) where

import Control.Monad.Except (throwError)
import Scheme.AST (Expr (ELambda), Id, mkId)
import Scheme.Analyzer (AnalyzeError (AnalyzeError), analyze)
import Scheme.Evaluator (applyMacro, evaluate, quoteToValue)
import Scheme.Runtime (
    Eval,
    EvalError (ArityError, OtherEvalError, TypeError),
    PairCell,
    Value (VBool, VClosure, VMacro, VNil, VNum, VPair, VStr, VSym),
    defineVar,
    lookupVarMaybe,
    pairSeen,
    showValueKind,
 )
import Scheme.SExpr (SExpr (SBool, SNil, SNum, SPair, SStr, SSym))

data ExpandedToplevel
    = ExpandedExpression SExpr
    | ExpandedMacroDefinition

{- | Runtime data that represents macro input syntax.

Macro transformers are ordinary Scheme code, so their inputs must be
runtime values. This wrapper marks values that are being used as syntax
datums rather than ordinary evaluated arguments.
-}
newtype SyntaxDatum = SyntaxDatum Value

-- | Runtime data returned by a macro transformer, expected to encode syntax.
newtype ExpansionResult = ExpansionResult Value

defaultExpansionLimit :: Int
defaultExpansionLimit = 1000

-- | Expand one top-level form and install top-level macro definitions.
expandToplevel :: SExpr -> Eval ExpandedToplevel
expandToplevel sexpr = do
    expanded <- expandWith defaultExpansionLimit sexpr
    case expanded of
        SPair (SSym "define-macro") rest -> do
            defineMacro rest
            pure ExpandedMacroDefinition
        _ -> pure $ ExpandedExpression expanded

expandWith :: Int -> SExpr -> Eval SExpr
expandWith fuel sexpr = case sexpr of
    SNum{} -> pure sexpr
    SBool{} -> pure sexpr
    SStr{} -> pure sexpr
    SSym{} -> pure sexpr
    SNil -> pure sexpr
    SPair (SSym name) args -> expandSymbolForm fuel name args sexpr
    SPair car cdr -> SPair <$> expandWith fuel car <*> expandPairTail fuel cdr

expandSymbolForm :: Int -> Text -> SExpr -> SExpr -> Eval SExpr
expandSymbolForm fuel name args original = case name of
    "quote" -> pure original
    "define-macro" -> pure original
    "lambda" -> expandLambdaForm fuel args original
    "define" -> expandDefineForm fuel args original
    "set!" -> expandSetForm fuel args original
    "let" -> expandLetForm fuel args original
    "let*" -> expandLetFamilyForm fuel name args original
    "letrec" -> expandLetFamilyForm fuel name args original
    "if" -> expandAllArgsForm fuel name args
    "cond" -> expandCondForm fuel args original
    "and" -> expandAllArgsForm fuel name args
    "or" -> expandAllArgsForm fuel name args
    "begin" -> expandAllArgsForm fuel name args
    "do" -> expandDoForm fuel args original
    "load" -> pure original
    _ -> do
        macro <- lookupMacro name
        case macro of
            Just transformer -> expandMacroCall fuel name args transformer
            Nothing -> do
                args' <- expandPairTail fuel args
                pure $ SPair (SSym name) args'

expandMacroCall :: Int -> Text -> SExpr -> Value -> Eval SExpr
expandMacroCall fuel name args transformer = do
    when
        (fuel <= 0)
        ( throwError $
            OtherEvalError
                ( "macro expansion exceeded "
                    <> show defaultExpansionLimit
                    <> " expansions"
                )
        )
    argumentSyntax <- requireProperList (name <> ": macro call") args
    argumentDatums <- traverse syntaxToDatum argumentSyntax
    result <- applyMacroToDatums transformer argumentDatums
    resultSyntax <- expansionResultToSyntax result
    expandWith (fuel - 1) resultSyntax

syntaxToDatum :: SExpr -> Eval SyntaxDatum
syntaxToDatum syntax =
    SyntaxDatum <$> quoteToValue syntax

applyMacroToDatums :: Value -> [SyntaxDatum] -> Eval ExpansionResult
applyMacroToDatums transformer datums =
    ExpansionResult <$> applyMacro transformer (datumValue <$> datums)
  where
    datumValue :: SyntaxDatum -> Value
    datumValue (SyntaxDatum value) = value

expansionResultToSyntax :: ExpansionResult -> Eval SExpr
expansionResultToSyntax (ExpansionResult value) =
    datumValueToSyntax value

lookupMacro :: Text -> Eval (Maybe Value)
lookupMacro name = case mkId name of
    Nothing -> pure Nothing
    Just ident -> do
        value <- lookupVarMaybe ident
        pure $ case value of
            Just macro@VMacro{} -> Just macro
            _ -> Nothing

defineMacro :: SExpr -> Eval ()
defineMacro rest = do
    args <- requireProperList "define-macro" rest
    case args of
        (SPair (SSym name) paramsSpec : body@(_ : _)) ->
            defineFunctionMacro name paramsSpec body
        [SSym name, transformerExpr] ->
            defineTransformerMacro name transformerExpr
        [SPair (SSym _) _] ->
            throwError $ OtherEvalError "define-macro: function body required"
        _ ->
            throwError $ OtherEvalError "define-macro: invalid syntax"

defineFunctionMacro :: Text -> SExpr -> [SExpr] -> Eval ()
defineFunctionMacro name paramsSpec body = do
    ident <- requireId "define-macro" name
    let lambdaSyntax = listToSExpr (SSym "lambda" : paramsSpec : body)
    expandedLambda <- expandWith defaultExpansionLimit lambdaSyntax
    case analyze expandedLambda of
        Left err -> throwAnalyzeError "define-macro" err
        Right (ELambda params macroBody) -> do
            env <- ask
            defineVar ident (VMacro params macroBody env)
        Right _ ->
            throwError $ OtherEvalError "define-macro: internal non-lambda transformer"

defineTransformerMacro :: Text -> SExpr -> Eval ()
defineTransformerMacro name transformerExpr = do
    ident <- requireId "define-macro" name
    expandedExpr <- expandWith defaultExpansionLimit transformerExpr
    expr <- case analyze expandedExpr of
        Left err -> throwAnalyzeError "define-macro" err
        Right analyzed -> pure analyzed
    value <- evaluate expr
    macro <- closureToMacro value
    defineVar ident macro

closureToMacro :: Value -> Eval Value
closureToMacro (VClosure params body closureEnv) = pure $ VMacro params body closureEnv
closureToMacro macro@VMacro{} = pure macro
closureToMacro value =
    throwError $
        TypeError
            ( "define-macro: expected transformer closure, got "
                <> showValueKind value
            )

expandLambdaForm :: Int -> SExpr -> SExpr -> Eval SExpr
expandLambdaForm fuel args original = case sexpToList args of
    Just (paramsSpec : body) -> do
        body' <- traverse (expandWith fuel) body
        pure $ listToSExpr (SSym "lambda" : paramsSpec : body')
    _ -> pure original

expandDefineForm :: Int -> SExpr -> SExpr -> Eval SExpr
expandDefineForm fuel args original = case sexpToList args of
    Just [target@SSym{}, rhs] -> do
        rhs' <- expandWith fuel rhs
        pure $ listToSExpr [SSym "define", target, rhs']
    Just (target@(SPair (SSym _) _) : body@(_ : _)) -> do
        body' <- traverse (expandWith fuel) body
        pure $ listToSExpr (SSym "define" : target : body')
    _ -> pure original

expandSetForm :: Int -> SExpr -> SExpr -> Eval SExpr
expandSetForm fuel args original = case sexpToList args of
    Just [target, rhs] -> do
        rhs' <- expandWith fuel rhs
        pure $ listToSExpr [SSym "set!", target, rhs']
    _ -> pure original

expandLetForm :: Int -> SExpr -> SExpr -> Eval SExpr
expandLetForm fuel args original = case sexpToList args of
    Just (name@SSym{} : bindings : body@(_ : _)) -> do
        bindings' <- expandBindings fuel bindings
        body' <- traverse (expandWith fuel) body
        pure $ listToSExpr (SSym "let" : name : bindings' : body')
    Just (bindings : body@(_ : _)) -> do
        bindings' <- expandBindings fuel bindings
        body' <- traverse (expandWith fuel) body
        pure $ listToSExpr (SSym "let" : bindings' : body')
    _ -> pure original

expandLetFamilyForm :: Int -> Text -> SExpr -> SExpr -> Eval SExpr
expandLetFamilyForm fuel name args original = case sexpToList args of
    Just (bindings : body@(_ : _)) -> do
        bindings' <- expandBindings fuel bindings
        body' <- traverse (expandWith fuel) body
        pure $ listToSExpr (SSym name : bindings' : body')
    _ -> pure original

expandBindings :: Int -> SExpr -> Eval SExpr
expandBindings fuel bindings = case sexpToList bindings of
    Just bindingList -> listToSExpr <$> traverse (expandBinding fuel) bindingList
    Nothing -> pure bindings

expandBinding :: Int -> SExpr -> Eval SExpr
expandBinding fuel binding = case sexpToList binding of
    Just [name, rhs] -> do
        rhs' <- expandWith fuel rhs
        pure $ listToSExpr [name, rhs']
    _ -> pure binding

expandCondForm :: Int -> SExpr -> SExpr -> Eval SExpr
expandCondForm fuel args original = case sexpToList args of
    Just clauses -> do
        clauses' <- traverse (expandCondClause fuel) clauses
        pure $ listToSExpr (SSym "cond" : clauses')
    Nothing -> pure original

expandCondClause :: Int -> SExpr -> Eval SExpr
expandCondClause fuel clause = case sexpToList clause of
    Just (elseSym@(SSym "else") : body) -> do
        body' <- traverse (expandWith fuel) body
        pure $ listToSExpr (elseSym : body')
    Just (test : body) -> do
        test' <- expandWith fuel test
        body' <- traverse (expandWith fuel) body
        pure $ listToSExpr (test' : body')
    _ -> pure clause

expandDoForm :: Int -> SExpr -> SExpr -> Eval SExpr
expandDoForm fuel args original = case sexpToList args of
    Just (bindings : testClause : body) -> do
        bindings' <- expandDoBindings fuel bindings
        testClause' <- expandDoTest fuel testClause
        body' <- traverse (expandWith fuel) body
        pure $ listToSExpr (SSym "do" : bindings' : testClause' : body')
    _ -> pure original

expandDoBindings :: Int -> SExpr -> Eval SExpr
expandDoBindings fuel bindings = case sexpToList bindings of
    Just bindingList -> listToSExpr <$> traverse (expandDoBinding fuel) bindingList
    Nothing -> pure bindings

expandDoBinding :: Int -> SExpr -> Eval SExpr
expandDoBinding fuel binding = case sexpToList binding of
    Just [name, initExpr, stepExpr] -> do
        initExpr' <- expandWith fuel initExpr
        stepExpr' <- expandWith fuel stepExpr
        pure $ listToSExpr [name, initExpr', stepExpr']
    _ -> pure binding

expandDoTest :: Int -> SExpr -> Eval SExpr
expandDoTest fuel testClause = case sexpToList testClause of
    Just (testExpr : results) -> do
        testExpr' <- expandWith fuel testExpr
        results' <- traverse (expandWith fuel) results
        pure $ listToSExpr (testExpr' : results')
    _ -> pure testClause

expandAllArgsForm :: Int -> Text -> SExpr -> Eval SExpr
expandAllArgsForm fuel name args = do
    args' <- expandPairTail fuel args
    pure $ SPair (SSym name) args'

expandPairTail :: Int -> SExpr -> Eval SExpr
expandPairTail _ SNil = pure SNil
expandPairTail fuel (SPair car cdr) =
    SPair <$> expandWith fuel car <*> expandPairTail fuel cdr
expandPairTail fuel atom = expandWith fuel atom

datumValueToSyntax :: Value -> Eval SExpr
datumValueToSyntax = go []
  where
    go :: [PairCell] -> Value -> Eval SExpr
    go _ (VNum n) = pure $ SNum n
    go _ (VBool b) = pure $ SBool b
    go _ (VStr s) = pure $ SStr s
    go _ (VSym s) = pure $ SSym s
    go _ VNil = pure SNil
    go seen (VPair carRef cdrRef) =
        let cell = (carRef, cdrRef)
         in if pairSeen cell seen
                then throwError $ OtherEvalError "macro result contains circular syntax"
                else do
                    carValue <- liftIO $ readIORef carRef
                    cdrValue <- liftIO $ readIORef cdrRef
                    SPair <$> go (cell : seen) carValue <*> go (cell : seen) cdrValue
    go _ value =
        throwError $
            TypeError
                ( "macro result: expected S-expression, got "
                    <> showValueKind value
                )

requireProperList :: Text -> SExpr -> Eval [SExpr]
requireProperList context sexpr = case sexpToList sexpr of
    Just values -> pure values
    Nothing -> throwError $ ArityError (context <> ": expected a proper list")

sexpToList :: SExpr -> Maybe [SExpr]
sexpToList SNil = Just []
sexpToList (SPair x rest) = (x :) <$> sexpToList rest
sexpToList _ = Nothing

listToSExpr :: [SExpr] -> SExpr
listToSExpr = foldr SPair SNil

requireId :: Text -> Text -> Eval Id
requireId context name = case mkId name of
    Just ident -> pure ident
    Nothing -> throwError $ OtherEvalError (context <> ": empty identifier")

throwAnalyzeError :: Text -> AnalyzeError -> Eval a
throwAnalyzeError context (AnalyzeError msg) =
    throwError $ OtherEvalError (context <> ": " <> msg)
