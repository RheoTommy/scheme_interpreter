module Scheme.Parser (parseSExpr, parseFile, ParseError, prettyError) where

import Data.Char qualified as Char
import Data.Text qualified as T
import Scheme.SExpr (SExpr (SBool, SNil, SNum, SPair, SStr, SSym))
import Text.Megaparsec (
    Parsec,
    between,
    choice,
    eof,
    notFollowedBy,
    satisfy,
    single,
    takeWhile1P,
    try,
    (<?>),
 )
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

-- | Parse error type exposed to callers.
type ParseError = M.ParseErrorBundle Text Void

-- | Render a parse error as a human-readable string.
prettyError :: ParseError -> String
prettyError = M.errorBundlePretty

-- | Parser type: parses Text with no custom error component.
type Parser = Parsec Void Text

-- | Parse a single S-expression from Text.
parseSExpr :: Text -> Either ParseError SExpr
parseSExpr = M.parse (sc *> sExpr <* eof) "<input>"

-- | Parse a file containing multiple S-expressions.
parseFile :: FilePath -> Text -> Either ParseError [SExpr]
parseFile = M.parse (sc *> many sExpr <* eof)

-- * Lexer

-- | Skip whitespace and line comments (; to end of line).
sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") M.empty

-- | Wrap a parser to consume trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse an exact symbol and consume trailing whitespace.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- * S-expression parsers

sExpr :: Parser SExpr
sExpr =
    lexeme
        ( choice
            [ try sNum -- may consume '-' before failing, needs backtrack
            , sBool -- '#' is unambiguous in Mini-Scheme
            , sStr -- '"' is unambiguous
            , sQuote -- '\'' is unambiguous
            , sList -- '(' is unambiguous
            , sSym -- fallback: consumes any identifier characters
            ]
            <?> "S-expression"
        )

{- | Integer literal. Tries to parse a signed decimal integer,
and ensures it is not followed by identifier characters.
TODO: Support hex (#x), octal (#o), binary (#b) literals if needed.
-}
sNum :: Parser SExpr
sNum = do
    sign <- M.optional (choice [single '-', single '+'])
    digits <- takeWhile1P (Just "digit") Char.isDigit
    notFollowedBy (satisfy isIdChar)
    let numText = maybe digits (\s -> if s == '+' then digits else "-" <> digits) sign
    -- readMaybe always succeeds here since numText matches [-]?[0-9]+,
    -- but we handle Nothing for totality.
    case readMaybe (T.unpack numText) of
        Just n -> pure (SNum n)
        Nothing -> fail $ "Invalid number: " <> T.unpack numText

-- | Boolean literal: #t or #f.
sBool :: Parser SExpr
sBool = do
    _ <- single '#'
    b <-
        choice
            [ True <$ single 't'
            , False <$ single 'f'
            ]
    notFollowedBy (satisfy isIdChar)
    pure $ SBool b

{- | String literal with escape sequences.
R5RS only requires \\ and \", but we also support \n, \t, \r for convenience.
-}
sStr :: Parser SExpr
sStr = SStr . T.pack <$> between (single '"') (single '"') (many stringChar)
  where
    stringChar :: Parser Char
    stringChar =
        choice
            [ satisfy (\c -> c /= '\\' && c /= '"' && c /= '\n' && c /= '\r')
            , single '\\' *> escapeChar
            ]

    escapeChar :: Parser Char
    escapeChar =
        choice
            [ '\n' <$ single 'n' -- line feed
            , '\t' <$ single 't' -- tab
            , '\\' <$ single '\\' -- backslash
            , '"' <$ single '"' -- double quote
            , '\r' <$ single 'r' -- carriage return
            ]
            <?> "escape character"

-- | Quote sugar: 'X is desugared to (quote X) at parse time.
sQuote :: Parser SExpr
sQuote = do
    _ <- single '\''
    sc
    x <- sExpr
    pure $ SPair (SSym "quote") (SPair x SNil)

-- | List or dotted pair: (S-Exp* [S-Exp . S-Exp])
sList :: Parser SExpr
sList = between (symbol "(") (single ')') $ do
    elems <- many (notFollowedBy dot *> sExpr)
    mDot <- M.optional dot
    case mDot of
        Nothing -> pure $ foldr SPair SNil elems
        Just _ -> do
            when (null elems) $ fail "Expected datum before dot"
            (\tail -> foldr SPair tail elems) <$> sExpr
  where
    -- A dot separator: a '.' not followed by identifier characters.
    dot :: Parser Char
    dot = try $ lexeme (single '.' <* notFollowedBy (satisfy isIdChar))

-- | Symbol (identifier). Rejects tokens that parse as numbers.
sSym :: Parser SExpr
sSym = do
    name <- takeWhile1P (Just "identifier character") isIdChar
    when (isJust (readMaybe @Integer (T.unpack name))) $
        fail "Expected identifier, got number"
    -- Bare "." is not a valid symbol; it is only used as a dot separator in lists.
    when (name == ".") $
        fail "Unexpected dot outside of list"
    pure $ SSym name

-- | Characters allowed in identifiers: [0-9A-Za-z!$%&*+-./<=>?@^_]
isIdChar :: Char -> Bool
isIdChar c =
    Char.isAsciiUpper c
        || Char.isAsciiLower c
        || Char.isDigit c
        || c `elem` ("!$%&*+-./<=>?@^_" :: [Char])
