module Scheme.Number (
    Number (NInteger, NRational, NInexactReal),
    numberDiv,
    numberIsZero,
    numberToText,
    parseNumberText,
) where

import Data.Char qualified as Char
import Data.Text qualified as T

data Number
    = NInteger Integer
    | NRational Rational
    | NInexactReal Double
    deriving stock (Show)

instance Eq Number where
    NInexactReal a == NInexactReal b = a == b
    NInexactReal a == b = a == numberToDouble b
    a == NInexactReal b = numberToDouble a == b
    a == b = exactToRational a == exactToRational b

instance Ord Number where
    compare (NInexactReal a) (NInexactReal b) = compare a b
    compare (NInexactReal a) b = compare a (numberToDouble b)
    compare a (NInexactReal b) = compare (numberToDouble a) b
    compare a b = compare (exactToRational a) (exactToRational b)

instance Num Number where
    NInexactReal a + b = NInexactReal (a + numberToDouble b)
    a + NInexactReal b = NInexactReal (numberToDouble a + b)
    a + b = normalizeExact (exactToRational a + exactToRational b)

    NInexactReal a - b = NInexactReal (a - numberToDouble b)
    a - NInexactReal b = NInexactReal (numberToDouble a - b)
    a - b = normalizeExact (exactToRational a - exactToRational b)

    NInexactReal a * b = NInexactReal (a * numberToDouble b)
    a * NInexactReal b = NInexactReal (numberToDouble a * b)
    a * b = normalizeExact (exactToRational a * exactToRational b)

    negate (NInteger n) = NInteger (negate n)
    negate (NRational n) = normalizeExact (negate n)
    negate (NInexactReal n) = NInexactReal (negate n)

    abs (NInteger n) = NInteger (abs n)
    abs (NRational n) = normalizeExact (abs n)
    abs (NInexactReal n) = NInexactReal (abs n)

    signum (NInteger n) = NInteger (signum n)
    signum (NRational n) = normalizeExact (signum n)
    signum (NInexactReal n) = NInexactReal (signum n)

    fromInteger = NInteger

numberDiv :: Number -> Number -> Maybe Number
numberDiv _ divisor
    | numberIsZero divisor = Nothing
numberDiv (NInexactReal a) b = Just $ NInexactReal (a / numberToDouble b)
numberDiv a (NInexactReal b) = Just $ NInexactReal (numberToDouble a / b)
numberDiv a b = Just $ normalizeExact (exactToRational a / exactToRational b)

numberIsZero :: Number -> Bool
numberIsZero (NInteger 0) = True
numberIsZero (NRational 0) = True
numberIsZero (NInexactReal 0) = True
numberIsZero _ = False

numberToText :: Number -> Text
numberToText (NInteger n) = show n
numberToText (NRational n) =
    show (numerator n) <> "/" <> show (denominator n)
numberToText (NInexactReal n) = show n

parseNumberText :: Text -> Maybe Number
parseNumberText input
    | isIntegerSyntax input = NInteger <$> readMaybe (T.unpack (dropLeadingPlus input))
    | otherwise = parseRationalText input <|> parseInexactRealText input

parseRationalText :: Text -> Maybe Number
parseRationalText input = do
    (isNegative, unsigned) <- stripSign input
    (rawNumerator, rawDenominator) <- splitRational unsigned
    guard (isDigits rawNumerator && isDigits rawDenominator)
    n <- readMaybe (T.unpack rawNumerator)
    d <- readMaybe (T.unpack rawDenominator)
    guard (d /= 0)
    let signedNumerator = if isNegative then negate n else n
    pure $ normalizeExact (fromInteger signedNumerator / fromInteger d)

parseInexactRealText :: Text -> Maybe Number
parseInexactRealText input
    | isDecimalSyntax input = NInexactReal <$> readMaybe (T.unpack (dropLeadingPlus input))
    | otherwise = Nothing

numberToDouble :: Number -> Double
numberToDouble (NInteger n) = fromInteger n
numberToDouble (NRational n) = fromRational n
numberToDouble (NInexactReal n) = n

exactToRational :: Number -> Rational
exactToRational (NInteger n) = fromInteger n
exactToRational (NRational n) = n
exactToRational (NInexactReal n) = toRational n

normalizeExact :: Rational -> Number
normalizeExact n
    | denominator n == 1 = NInteger (numerator n)
    | otherwise = NRational n

dropLeadingPlus :: Text -> Text
dropLeadingPlus input = fromMaybe input (T.stripPrefix "+" input)

isIntegerSyntax :: Text -> Bool
isIntegerSyntax input =
    let unsigned = dropOptionalSign input
     in isDigits unsigned

isDecimalSyntax :: Text -> Bool
isDecimalSyntax input =
    let unsigned = dropOptionalSign input
     in case T.splitOn "." unsigned of
            [whole, fraction] ->
                isDigits whole && isDigits fraction
            _ -> False

isDigits :: Text -> Bool
isDigits input = not (T.null input) && T.all Char.isDigit input

splitRational :: Text -> Maybe (Text, Text)
splitRational input = case T.splitOn "/" input of
    [rawNumerator, rawDenominator] -> Just (rawNumerator, rawDenominator)
    _ -> Nothing

stripSign :: Text -> Maybe (Bool, Text)
stripSign input
    | T.null input = Nothing
    | Just unsigned <- T.stripPrefix "-" input = Just (True, unsigned)
    | Just unsigned <- T.stripPrefix "+" input = Just (False, unsigned)
    | otherwise = Just (False, input)

dropOptionalSign :: Text -> Text
dropOptionalSign input =
    fromMaybe input $
        T.stripPrefix "+" input <|> T.stripPrefix "-" input
