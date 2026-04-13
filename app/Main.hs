module Main (main) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
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
            let input = toText . dropWhileEnd isSpace . dropWhile isSpace $ toString line
            unless (input == ":q") $ do
                unless (input == "") $
                    case parseSExpr input of
                        Right sexpr -> print sexpr
                        Left err -> putStrLn (prettyError err)
                repl
