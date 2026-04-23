module Main (main) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
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
            let input = toText . dropWhileEnd isSpace . dropWhile isSpace $ toString line
            unless (input == ":q") $ do
                unless (input == "") $ do
                    result <- runIn env input
                    case result of
                        Right output -> putTextLn output
                        Left err -> putTextLn err
                repl env
