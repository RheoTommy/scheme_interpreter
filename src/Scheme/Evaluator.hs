{- | Evaluator: 'Expr' -> 'Value' in the 'Eval' monad.

This is the first step of the evaluator. Only the following forms are
implemented:

  * Literals ('ELit')
  * Variable references ('EVar')
  * Function application ('EApp')

All other forms (lambda, let, if, ...) raise an error "not yet
implemented" so they can be added incrementally.

Responsibility is deliberately limited to @Expr -> Value@: parsing and
analysis live upstream in 'Scheme.Parser' and 'Scheme.Analyzer', and
the end-to-end @Text -> Value@ driver lives in 'Scheme.Interpreter'.
-}
module Scheme.Evaluator (
    evaluate,
    initialEnv,
) where

import Control.Monad.Except (throwError)
import Data.Map.Strict qualified as Map
import Scheme.AST (
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
    Literal (LBool, LNil, LNum, LStr),
    mkId,
 )
import Scheme.Builtin (builtins)
import Scheme.Runtime (
    Env,
    Eval,
    EvalError (NotAProcedure, NotImplemented),
    Value (VBool, VBuiltin, VNil, VNum, VStr),
    bindInFrame,
    lookupVar,
    newFrame,
    showValueKind,
 )

-- | Evaluate a single expression in the 'Eval' monad.
evaluate :: Expr -> Eval Value
evaluate expr = case expr of
    ELit lit -> pure (literalToValue lit)
    EVar ident -> lookupVar ident
    EApp fExpr argExprs -> do
        -- Function and arguments are evaluated left-to-right, following
        -- 'traverse's sequencing in 'Eval'.
        f <- evaluate fExpr
        args <- traverse evaluate argExprs
        apply f args
    ELambda{} -> notImpl "lambda"
    EQuote{} -> notImpl "quote"
    ESet{} -> notImpl "set!"
    ELet{} -> notImpl "let"
    ELetStar{} -> notImpl "let*"
    ELetrec{} -> notImpl "letrec"
    EIf{} -> notImpl "if"
    ECond{} -> notImpl "cond"
    EAnd{} -> notImpl "and"
    EOr{} -> notImpl "or"
    EBegin{} -> notImpl "begin"
    EDo{} -> notImpl "do"
  where
    notImpl :: Text -> Eval a
    notImpl form = throwError (NotImplemented form)

literalToValue :: Literal -> Value
literalToValue lit = case lit of
    LNum n -> VNum n
    LBool b -> VBool b
    LStr s -> VStr s
    LNil -> VNil

-- | Apply a value to a list of argument values (function call).
apply :: Value -> [Value] -> Eval Value
apply (VBuiltin _ f) args = f args
apply v _ = throwError $ NotAProcedure (showValueKind v)

-- | Build a top-level environment populated with all builtins.
initialEnv :: IO Env
initialEnv = do
    frame <- newFrame
    for_ (Map.toList builtins) $ \(name, v) -> case mkId name of
        Just ident -> bindInFrame ident v frame
        -- Builtin names are static and known-valid identifiers; an empty
        -- name here indicates a programming error in 'Scheme.Builtin'.
        Nothing -> error ("initialEnv: invalid builtin name: " <> name)
    pure frame
