module Main (main) where

import Data.Text qualified as T
import Scheme.Interpreter (Env, initialEnv, runIn)

main :: IO ()
main = do
    putTextLn "Mini-Scheme REPL"
    env <- initialEnv
    repl env

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
