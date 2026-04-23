{- | End-to-end interpreter: coordinates Parser, Analyzer, and Evaluator.

This module is the only place that depends on all pipeline stages.
Keeping it separate from 'Scheme.Evaluator' lets the evaluator itself
stay focused on @Expr -> Value@ and avoid a dependency on the parser.

Two entry points are provided:

  * 'runIn' takes an explicit 'Env' and is suitable for an interactive
    REPL or any caller that wants to accumulate state across evaluations.
    The environment is mutated in place via 'IORef'; the same 'Env'
    reference keeps observing later updates.
  * 'run' is a convenience wrapper that creates a fresh environment for
    a single evaluation, suitable for tests and one-shot scripts.
-}
module Scheme.Interpreter (
    Env,
    initialEnv,
    run,
    runIn,
) where

import Scheme.AST (Toplevel (TDefine, TExpr, TLoad))
import Scheme.Analyzer (AnalyzeError (AnalyzeError), analyzeToplevel)
import Scheme.Evaluator (evaluate, initialEnv)
import Scheme.Parser (parseSExpr, prettyError)
import Scheme.Runtime (Env, prettyEvalError, runEval, showValue)

{- | Parse, analyze, and evaluate a single top-level form in the given
environment.

Returns the displayed result on success, or a human-readable error
message on failure at any pipeline stage.
-}
runIn :: Env -> Text -> IO (Either Text Text)
runIn env input = case parseSExpr input of
    Left err -> pure $ Left ("parse error: " <> toText (prettyError err))
    Right sexpr -> case analyzeToplevel sexpr of
        Left (AnalyzeError msg) -> pure $ Left ("analyze error: " <> msg)
        Right (TExpr expr) -> do
            result <- runEval env (evaluate expr)
            pure $ case result of
                Left err -> Left ("eval error: " <> prettyEvalError err)
                Right v -> Right (showValue v)
        Right TDefine{} -> pure $ Left "top-level define: not yet implemented"
        Right TLoad{} -> pure $ Left "load: not yet implemented"

{- | Convenience wrapper: parse, analyze, and evaluate a single top-level
form in a fresh environment. Equivalent to @initialEnv >>= flip runIn input@.
-}
run :: Text -> IO (Either Text Text)
run input = do
    env <- initialEnv
    runIn env input
