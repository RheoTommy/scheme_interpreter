module Main (main) where

import Control.Exception (AsyncException (UserInterrupt), catch, throwIO)
import Data.Text qualified as T
import Scheme.Interpreter (Env, initialEnv, runFileIn, runIn)

data ReplStep
    = ReplContinue
    | ReplStop

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> startRepl
        ["-h"] -> printUsage
        ["--help"] -> printUsage
        [path] -> runFile path
        _ -> printUsage *> exitFailure

startRepl :: IO ()
startRepl = do
    putTextLn "Mini-Scheme REPL"
    env <- initialEnv
    repl env

runFile :: FilePath -> IO ()
runFile path = do
    result <-
        catchUserInterrupt $ do
            env <- initialEnv
            runFileIn env path
    case result of
        Nothing -> putTextLn "Interrupted." *> exitFailure
        Just (Left err) -> putTextLn err *> exitFailure
        Just (Right outputs) -> traverse_ putTextLn outputs

printUsage :: IO ()
printUsage = do
    putTextLn "Usage: scheme-interpreter [FILE]"
    putTextLn "Run without FILE to start the REPL."

repl :: Env -> IO ()
repl env = do
    result <- catchUserInterrupt (replStep env)
    case result of
        Nothing -> do
            putTextLn ""
            putTextLn "Interrupted."
            repl env
        Just ReplContinue -> repl env
        Just ReplStop -> pure ()

replStep :: Env -> IO ReplStep
replStep env = do
    putText "scheme> "
    hFlush stdout
    eof <- hIsEOF stdin
    if eof
        then putTextLn "" $> ReplStop
        else do
            line <- getLine
            let input = T.strip line
            if input == ":q"
                then pure ReplStop
                else do
                    unless (T.null input) $ do
                        result <- runIn env input
                        case result of
                            Right output -> putTextLn output
                            Left err -> putTextLn err
                    pure ReplContinue

catchUserInterrupt :: IO a -> IO (Maybe a)
catchUserInterrupt action =
    catch (Just <$> action) handleInterrupt
  where
    handleInterrupt UserInterrupt = pure Nothing
    handleInterrupt asyncException = throwIO asyncException
