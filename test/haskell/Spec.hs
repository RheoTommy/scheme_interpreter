module Main (main) where

import Data.Text qualified as T
import Sample.Interpreter (run)
import Scheme.Parser (parseFile, parseSExpr)
import Scheme.SExpr (SExpr (SBool, SNil, SNum, SPair, SStr, SSym))
import Test.HUnit (
    Counts (errors, failures),
    Test (TestList),
    assertEqual,
    assertFailure,
    runTestTT,
    (~:),
 )

-- * Test helpers

-- | Assert that parsing input produces the expected SExpr.
testParse :: Text -> SExpr -> Test
testParse input expected =
    T.unpack input ~: case parseSExpr input of
        Right actual -> assertEqual "" expected actual
        Left err -> assertFailure $ "Parse failed: " <> show err

-- | Assert that parsing input fails.
testParseFail :: Text -> Test
testParseFail input =
    (T.unpack input <> " => FAIL") ~: case parseSExpr input of
        Left _ -> pure ()
        Right v -> assertFailure $ "Expected parse error, got: " <> show v

-- | Assert that parsing a file produces the expected list of SExprs.
testParseFile :: Text -> [SExpr] -> Test
testParseFile input expected =
    T.unpack input ~: case parseFile "<test>" input of
        Right actual -> assertEqual "" expected actual
        Left err -> assertFailure $ "Parse failed: " <> show err

-- | Assert that evaluating input produces the expected output string.
testEval :: Text -> Text -> Test
testEval input expected =
    T.unpack input ~: case run input of
        Right actual -> assertEqual "" expected actual
        Left err -> assertFailure $ "Expected " <> T.unpack expected <> ", got error: " <> T.unpack err

-- | Assert that evaluating input produces an error.
testError :: Text -> Test
testError input =
    (T.unpack input <> " => ERROR") ~: case run input of
        Left _ -> pure ()
        Right v -> assertFailure $ "Expected error, got: " <> T.unpack v

-- * Helper to build SExpr lists

-- | Build a proper list from a Haskell list of SExprs.
slist :: [SExpr] -> SExpr
slist = foldr SPair SNil

-- * Parser tests

parserNums :: Test
parserNums =
    "Parser: numbers"
        ~: TestList
            [ testParse "42" (SNum 42)
            , testParse "0" (SNum 0)
            , testParse "-1" (SNum (-1))
            , testParse "-42" (SNum (-42))
            , testParse "999999" (SNum 999999)
            , testParse "  42  " (SNum 42) -- whitespace
            , testParse "+42" (SNum 42)
            , testParse "+0" (SNum 0)
            ]

parserBools :: Test
parserBools =
    "Parser: booleans"
        ~: TestList
            [ testParse "#t" (SBool True)
            , testParse "#f" (SBool False)
            , testParseFail "#x" -- invalid boolean
            , testParseFail "#" -- incomplete boolean
            ]

parserStrings :: Test
parserStrings =
    "Parser: strings"
        ~: TestList
            [ testParse "\"hello\"" (SStr "hello")
            , testParse "\"\"" (SStr "")
            , testParse "\"hello world\"" (SStr "hello world")
            , testParse "\"escape: \\\" \\\\ \\n\"" (SStr "escape: \" \\ \n")
            , testParse "\"tab:\\there\"" (SStr "tab:\there")
            , testParse "\"cr:\\r\"" (SStr "cr:\r")
            , testParseFail "\"unterminated" -- unclosed string
            , testParseFail "\"line1\nline2\"" -- literal newline in string
            ]

parserSymbols :: Test
parserSymbols =
    "Parser: symbols"
        ~: TestList
            [ testParse "x" (SSym "x")
            , testParse "hello" (SSym "hello")
            , testParse "+" (SSym "+")
            , testParse "-" (SSym "-")
            , testParse "..." (SSym "...")
            , testParse "list->string" (SSym "list->string")
            , testParse "set!" (SSym "set!")
            , testParse "null?" (SSym "null?")
            , testParse "a1" (SSym "a1")
            , testParse "$x" (SSym "$x")
            , testParse "<=?" (SSym "<=?")
            ]

parserQuote :: Test
parserQuote =
    "Parser: quote"
        ~: TestList
            [ testParse "'x" (slist [SSym "quote", SSym "x"])
            , testParse "'42" (slist [SSym "quote", SNum 42])
            , testParse "'#t" (slist [SSym "quote", SBool True])
            , testParse "'(1 2 3)" (slist [SSym "quote", slist [SNum 1, SNum 2, SNum 3]])
            , testParse "''x" (slist [SSym "quote", slist [SSym "quote", SSym "x"]])
            , testParse "'()" (slist [SSym "quote", SNil])
            , testParse "' x" (slist [SSym "quote", SSym "x"]) -- space after quote
            , testParse "' ;comment\nx" (slist [SSym "quote", SSym "x"]) -- comment after quote
            ]

parserLists :: Test
parserLists =
    "Parser: lists"
        ~: TestList
            [ testParse "()" SNil
            , testParse "(1)" (slist [SNum 1])
            , testParse "(1 2 3)" (slist [SNum 1, SNum 2, SNum 3])
            , testParse "(+ 1 2)" (slist [SSym "+", SNum 1, SNum 2])
            , testParse "(a (b c) d)" (slist [SSym "a", slist [SSym "b", SSym "c"], SSym "d"])
            , testParse "(1 (2 (3)))" (slist [SNum 1, slist [SNum 2, slist [SNum 3]]])
            ]

parserDotPairs :: Test
parserDotPairs =
    "Parser: dotted pairs"
        ~: TestList
            [ testParse "(a . b)" (SPair (SSym "a") (SSym "b"))
            , testParse "(1 . 2)" (SPair (SNum 1) (SNum 2))
            , testParse "(a b . c)" (SPair (SSym "a") (SPair (SSym "b") (SSym "c")))
            , testParse "(a . (b . c))" (SPair (SSym "a") (SPair (SSym "b") (SSym "c")))
            , testParseFail "(. a)" -- no datum before dot
            , testParseFail "(a . b c)" -- extra datum after dot pair tail
            , testParseFail "(a .)" -- missing datum after dot
            ]

parserComments :: Test
parserComments =
    "Parser: comments"
        ~: TestList
            [ testParse "; comment\n42" (SNum 42)
            , testParse "42 ; trailing comment" (SNum 42)
            , testParse "; full line comment\n(+ 1 2)" (slist [SSym "+", SNum 1, SNum 2])
            ]

parserWhitespace :: Test
parserWhitespace =
    "Parser: whitespace handling"
        ~: TestList
            [ testParse "  42  " (SNum 42)
            , testParse "\n\t42\n" (SNum 42)
            , testParse "( 1  2   3 )" (slist [SNum 1, SNum 2, SNum 3])
            , testParseFail "" -- empty input
            , testParseFail "   " -- whitespace only
            ]

parserEdgeCases :: Test
parserEdgeCases =
    "Parser: edge cases"
        ~: TestList
            [ -- Identifier vs number disambiguation
              testParse "-" (SSym "-")
            , testParse "-a" (SSym "-a")
            , testParse "-3" (SNum (-3))
            , testParse "+3" (SNum 3)
            , testParse "+a" (SSym "+a")
            , testParse "1+" (SSym "1+")
            , testParse "1-" (SSym "1-")
            , -- Nested quotes and lists
              testParse "'(a . b)" (slist [SSym "quote", SPair (SSym "a") (SSym "b")])
            , testParse "(a . (b . ()))" (slist [SSym "a", SSym "b"])
            , -- Parse errors
              testParseFail "(" -- unclosed paren
            , testParseFail ")" -- unexpected close paren
            , testParseFail "(1 2" -- unclosed list
            , testParseFail "1 2" -- multiple exprs in parseSExpr
            , testParseFail "." -- bare dot is not a valid symbol
            ]

parserFile :: Test
parserFile =
    "Parser: parseFile"
        ~: TestList
            [ testParseFile "" []
            , testParseFile "42" [SNum 42]
            , testParseFile "1 2 3" [SNum 1, SNum 2, SNum 3]
            , testParseFile "(define x 1)\nx" [slist [SSym "define", SSym "x", SNum 1], SSym "x"]
            , testParseFile "; comment\n42 ; trailing\n#t" [SNum 42, SBool True]
            ]

-- * Sample interpreter tests (kept from before)

step0 :: Test
step0 =
    "Step 0: Literals"
        ~: TestList
            [ testEval "42" "42"
            , testEval "0" "0"
            , testEval "-1" "-1"
            ]

step1 :: Test
step1 =
    "Step 1: Arithmetic"
        ~: TestList
            [ testEval "(+ 1 2)" "3"
            , testEval "(+ 1 2 3)" "6"
            , testEval "(+)" "0"
            , testEval "(- 5 3)" "2"
            , testEval "(- 10 3 2)" "5"
            , testEval "(- 5)" "-5"
            , testEval "(* 2 3)" "6"
            , testEval "(* 2 3 4)" "24"
            , testEval "(*)" "1"
            , testEval "(/ 10 2)" "5"
            , testEval "(/ 10 3)" "3"
            , testEval "(+ 1 (* 2 3))" "7"
            , testEval "(* (+ 1 2) (+ 3 4))" "21"
            , testEval "(- (* 3 3) (* 2 2))" "5"
            , testError "(/ 10 0)"
            , testError "(-)"
            , testError "(/ 1)"
            , testError "(+ 1 x)"
            ]

main :: IO ()
main = do
    result <-
        runTestTT $
            TestList
                [ parserNums
                , parserBools
                , parserStrings
                , parserSymbols
                , parserQuote
                , parserLists
                , parserDotPairs
                , parserComments
                , parserWhitespace
                , parserEdgeCases
                , parserFile
                , step0
                , step1
                ]
    if errors result + failures result == 0
        then exitSuccess
        else exitFailure
