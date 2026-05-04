{- | End-to-end interpreter: coordinates Parser, Analyzer, and Evaluator.

This module is the only place that depends on all pipeline stages.
Keeping it separate from 'Scheme.Evaluator' lets the evaluator itself
stay focused on @Expr -> Value@ and avoid a dependency on the parser.

Two entry points are provided:

  * 'runIn' takes an explicit 'Env' and is suitable for an interactive
    REPL or any caller that wants state to accumulate across evaluations.
    The environment is shared by reference; later updates from prior
    calls remain visible.
  * 'run' is a convenience wrapper that creates a fresh environment for
    a single evaluation, suitable for tests and one-shot scripts.

Each call evaluates exactly one top-level form. Multi-form input is the
caller's responsibility (see 'Scheme.Parser.parseFile').
-}
module Scheme.Interpreter (
    Env,
    initialEnv,
    run,
    runIn,
) where

import Scheme.AST (Toplevel (TDefine, TExpr, TLoad))
import Scheme.Analyzer (AnalyzeError (AnalyzeError), analyzeToplevel)
import Scheme.Environment (initialEnv)
import Scheme.Evaluator (evaluate, evaluateDefine)
import Scheme.Parser (parseSExpr, prettyError)
import Scheme.Runtime (
    Env,
    EvalError (NotImplemented),
    prettyEvalError,
    runEval,
    showValueIO,
 )

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
            formatEvalResult result
        Right (TDefine define) -> do
            result <- runEval env (evaluateDefine define)
            formatEvalResult result
        Right TLoad{} -> pure $ Left (notImplementedMsg "load")
  where
    formatEvalResult result = case result of
        Left err -> pure $ Left ("eval error: " <> prettyEvalError err)
        Right v -> Right <$> showValueIO v

    notImplementedMsg form = "eval error: " <> prettyEvalError (NotImplemented form)

{- | Convenience wrapper: parse, analyze, and evaluate a single top-level
form in a fresh environment. Equivalent to @initialEnv >>= flip runIn input@.
-}
run :: Text -> IO (Either Text Text)
run input = do
    env <- initialEnv
    runIn env input
