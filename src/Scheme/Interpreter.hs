{- | End-to-end interpreter: coordinates Parser, Analyzer, and Evaluator.

This module is the only place that depends on all pipeline stages.
Keeping it separate from 'Scheme.Evaluator' lets the evaluator itself
stay focused on @Expr -> Value@ and avoid a dependency on the parser.
-}
module Scheme.Interpreter (run) where

import Scheme.AST (Toplevel (TDefine, TExpr, TLoad))
import Scheme.Analyzer (AnalyzeError (AnalyzeError), analyzeToplevel)
import Scheme.Evaluator (evaluate, initialEnv)
import Scheme.Parser (parseSExpr, prettyError)
import Scheme.Runtime (prettyEvalError, runEval, showValue)

{- | Parse, analyze, and evaluate a single top-level form.

Returns the displayed result on success, or a human-readable error
message on failure at any stage.
-}
run :: Text -> IO (Either Text Text)
run input = case parseSExpr input of
    Left err -> pure $ Left ("parse error: " <> toText (prettyError err))
    Right sexpr -> case analyzeToplevel sexpr of
        Left (AnalyzeError msg) -> pure $ Left ("analyze error: " <> msg)
        Right (TExpr expr) -> do
            env <- initialEnv
            result <- runEval env (evaluate expr)
            pure $ case result of
                Left err -> Left ("eval error: " <> prettyEvalError err)
                Right v -> Right (showValue v)
        Right TDefine{} -> pure $ Left "top-level define: not yet implemented"
        Right TLoad{} -> pure $ Left "load: not yet implemented"
