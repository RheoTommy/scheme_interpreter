module Main (main) where

import Data.Text qualified as T
import Scheme.Interpreter (Env, initialEnv, runFileIn, runIn)

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
    env <- initialEnv
    result <- runFileIn env path
    case result of
        Left err -> putTextLn err *> exitFailure
        Right outputs -> traverse_ putTextLn outputs

printUsage :: IO ()
printUsage = do
    putTextLn "Usage: scheme-interpreter [FILE]"
    putTextLn "Run without FILE to start the REPL."

repl :: Env -> IO ()
repl env = do
    putText "scheme> "
    hFlush stdout
    eof <- hIsEOF stdin
    if eof
        then putTextLn ""
        else do
            line <- getLine
            let input = T.strip line
            unless (input == ":q") $ do
                unless (T.null input) $ do
                    result <- runIn env input
                    case result of
                        Right output -> putTextLn output
                        Left err -> putTextLn err
                repl env
