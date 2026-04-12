module Sample.Parser (parse) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.Text qualified as T
import Sample.Types (SExpr (SNil, SNum, SPair, SSym))

-- | Parse a Text into an S-expression.
parse :: Text -> Either Text SExpr
parse input = case parseSExpr (T.stripStart input) of
    Right (expr, rest)
        | T.all isSpace rest -> Right expr
        | otherwise -> Left $ "Unexpected trailing input: " <> rest
    Left err -> Left err

parseSExpr :: Text -> Either Text (SExpr, Text)
parseSExpr t
    | T.null t = Left "Unexpected end of input"
    | T.head t == '(' = parseList (T.stripStart (T.tail t))
    | isNumStart t =
        let (numStr, rest) = T.span isNumChar t
         in case readMaybe (T.unpack numStr) of
                Just n -> Right (SNum n, rest)
                Nothing -> Left $ "Invalid number: " <> numStr
    | isSymChar (T.head t) =
        let (sym, rest) = T.span isSymChar t
         in Right (SSym sym, rest)
    | otherwise = Left $ "Unexpected character: " <> T.singleton (T.head t)

parseList :: Text -> Either Text (SExpr, Text)
parseList t
    | T.null t = Left "Unexpected end of input: unclosed list"
    | T.head t == ')' = Right (SNil, T.drop 1 t)
    | otherwise = do
        (car, rest1) <- parseSExpr t
        (cdr, rest2) <- parseList (T.stripStart rest1)
        Right (SPair car cdr, rest2)

isNumStart :: Text -> Bool
isNumStart t = case T.uncons t of
    Just ('-', rest) -> case T.uncons rest of
        Just (d, _) -> isDigit d
        Nothing -> False
    Just (c, _) -> isDigit c
    Nothing -> False

isNumChar :: Char -> Bool
isNumChar c = isDigit c || c == '-'

isSymChar :: Char -> Bool
isSymChar c =
    c `elem` ("+-*/<>=!?_" :: [Char])
        || isAsciiLower c
        || isAsciiUpper c
