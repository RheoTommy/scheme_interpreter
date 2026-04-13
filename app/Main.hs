module Main (main) where

import Scheme.Parser (parseSExpr, prettyError)

main :: IO ()
main = do
    putTextLn "Mini-Scheme REPL (parser only)"
    repl

repl :: IO ()
repl = do
    putText "scheme> "
    hFlush stdout
    eof <- hIsEOF stdin
    if eof
        then putTextLn ""
        else do
            line <- getLine
            unless (line == ":q") $ do
                case parseSExpr line of
                    Right sexpr -> print sexpr
                    Left err -> putStrLn (prettyError err)
                repl
