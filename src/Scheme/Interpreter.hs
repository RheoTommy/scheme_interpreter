{- | End-to-end interpreter: coordinates Parser, Analyzer, and Evaluator.

This module is the only place that depends on all pipeline stages.
Keeping it separate from 'Scheme.Evaluator' lets the evaluator itself
stay focused on @Expr -> Value@ and avoid a dependency on the parser.

Public entry points:

  * 'runIn' takes an explicit 'Env' and is suitable for an interactive
    REPL or any caller that wants state to accumulate across evaluations.
    The environment is shared by reference; later updates from prior
    calls remain visible.
  * 'runSourceIn' evaluates a text containing zero or more top-level forms
    in order, using the same environment throughout.
  * 'runFileIn' reads a Scheme source file and evaluates all top-level
    forms in order. It is also the implementation behind top-level @load@.
  * 'run' is a convenience wrapper that creates a fresh environment for
    a single evaluation, suitable for tests and one-shot scripts.

'runIn' still evaluates exactly one top-level form; use 'runSourceIn' or
'runFileIn' for multi-form source.
-}
module Scheme.Interpreter (
    Env,
    initialEnv,
    run,
    runFileIn,
    runIn,
    runSourceIn,
) where

import Control.Exception (IOException, try)
import Data.Text.IO qualified as TIO
import Scheme.AST (Toplevel (TDefine, TExpr, TLoad))
import Scheme.Analyzer (AnalyzeError (AnalyzeError), analyzeToplevel)
import Scheme.Environment (initialEnv)
import Scheme.Evaluator (evaluate, evaluateDefine)
import Scheme.Parser (parseFile, parseSExpr, prettyError)
import Scheme.Runtime (
    Env,
    EvalError,
    Value (VUnspecified),
    prettyEvalError,
    runEval,
    showValueIO,
 )
import System.FilePath (isRelative, takeDirectory, (</>))

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
        Right toplevel -> evalToplevelIn "." env toplevel

{- | Convenience wrapper: parse, analyze, and evaluate a single top-level
form in a fresh environment. Equivalent to @initialEnv >>= flip runIn input@.
-}
run :: Text -> IO (Either Text Text)
run input = do
    env <- initialEnv
    runIn env input

-- | Parse and evaluate zero or more top-level forms in a shared environment.
runSourceIn :: Env -> Text -> IO (Either Text [Text])
runSourceIn = runSourceWithBaseDir "." "<input>"

-- | Read and evaluate a Scheme source file in a shared environment.
runFileIn :: Env -> FilePath -> IO (Either Text [Text])
runFileIn env path = do
    result <- readSourceFile path
    case result of
        Left err -> pure $ Left err
        Right input -> runSourceWithBaseDir (takeDirectory path) path env input

runSourceWithBaseDir :: FilePath -> FilePath -> Env -> Text -> IO (Either Text [Text])
runSourceWithBaseDir baseDir sourceName env input = case parseFile sourceName input of
    Left err -> pure $ Left ("parse error: " <> toText (prettyError err))
    Right sexprs -> case traverse analyzeToplevel sexprs of
        Left (AnalyzeError msg) -> pure $ Left ("analyze error: " <> msg)
        Right toplevels -> evalToplevels baseDir env toplevels

evalToplevels :: FilePath -> Env -> [Toplevel] -> IO (Either Text [Text])
evalToplevels _ _ [] = pure $ Right []
evalToplevels baseDir env (toplevel : rest) = do
    result <- evalToplevelIn baseDir env toplevel
    case result of
        Left err -> pure $ Left err
        Right output -> do
            restResult <- evalToplevels baseDir env rest
            pure $ (output :) <$> restResult

evalToplevelIn :: FilePath -> Env -> Toplevel -> IO (Either Text Text)
evalToplevelIn baseDir env toplevel = case toplevel of
    TExpr expr -> do
        result <- runEval env (evaluate expr)
        formatEvalResult result
    TDefine define -> do
        result <- runEval env (evaluateDefine define)
        formatEvalResult result
    TLoad path -> do
        result <- runFileIn env (resolvePath baseDir path)
        case result of
            Left err -> pure $ Left err
            Right _ -> Right <$> showValueIO VUnspecified

formatEvalResult :: Either EvalError Value -> IO (Either Text Text)
formatEvalResult result = case result of
    Left err -> pure $ Left ("eval error: " <> prettyEvalError err)
    Right v -> Right <$> showValueIO v

resolvePath :: FilePath -> Text -> FilePath
resolvePath baseDir pathText =
    let path = toString pathText
     in if isRelative path then baseDir </> path else path

readSourceFile :: FilePath -> IO (Either Text Text)
readSourceFile path = do
    result <- tryReadTextFile path
    pure $ case result of
        Left err -> Left ("io error: " <> toText path <> ": " <> show err)
        Right input -> Right input

tryReadTextFile :: FilePath -> IO (Either IOException Text)
tryReadTextFile path = try (TIO.readFile path)
