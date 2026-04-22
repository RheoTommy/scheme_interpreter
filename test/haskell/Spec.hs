module Main (main) where

import Data.Text qualified as T
import Sample.Interpreter (run)
import Scheme.AST (
    Body (Body),
    CondClauses (CondClauses, CondElseOnly),
    Define (Define),
    Expr (EAnd, EApp, EBegin, ECond, EDo, EIf, ELambda, ELet, ELetStar, ELetrec, ELit, EOr, EQuote, ESet, EVar),
    Id,
    Literal (LBool, LNil, LNum, LStr),
    Params (Params),
    Toplevel (TDefine, TLoad),
    mkId,
 )
import Scheme.Analyzer (analyze, analyzeToplevel)
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
            , testParseFail "\955" -- λ (Greek letter)
            , testParseFail "\1633\1634" -- ١٢ (Arabic-Indic digits)
            , testParseFail "\12354" -- あ (Hiragana)
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

-- * Analyzer test helpers

-- | Unsafe mkId for tests — panics on empty input.
id' :: Text -> Id
id' t = case mkId t of
    Just i -> i
    Nothing -> error "id': empty identifier in test"

-- | Parse then analyze, compare with expected Expr.
testAnalyze :: Text -> Expr -> Test
testAnalyze input expected =
    T.unpack input ~: case parseSExpr input of
        Left err -> assertFailure $ "Parse failed: " <> show err
        Right sexpr -> case analyze sexpr of
            Right actual -> assertEqual "" expected actual
            Left err -> assertFailure $ "Analyze failed: " <> show err

-- | Parse then analyzeToplevel, compare with expected Toplevel.
testAnalyzeTL :: Text -> Toplevel -> Test
testAnalyzeTL input expected =
    T.unpack input ~: case parseSExpr input of
        Left err -> assertFailure $ "Parse failed: " <> show err
        Right sexpr -> case analyzeToplevel sexpr of
            Right actual -> assertEqual "" expected actual
            Left err -> assertFailure $ "Analyze failed: " <> show err

-- | Parse then analyze, expect failure.
testAnalyzeFail :: Text -> Test
testAnalyzeFail input =
    (T.unpack input <> " => FAIL") ~: case parseSExpr input of
        Left err -> assertFailure $ "Parse failed (expected analyze error): " <> show err
        Right sexpr -> case analyze sexpr of
            Left _ -> pure ()
            Right v -> assertFailure $ "Expected analyze error, got: " <> show v

-- | Parse then analyzeToplevel, expect failure.
testAnalyzeTLFail :: Text -> Test
testAnalyzeTLFail input =
    (T.unpack input <> " => FAIL") ~: case parseSExpr input of
        Left err -> assertFailure $ "Parse failed (expected analyze error): " <> show err
        Right sexpr -> case analyzeToplevel sexpr of
            Left _ -> pure ()
            Right v -> assertFailure $ "Expected analyze error, got: " <> show v

-- * Analyzer tests

analyzerLiterals :: Test
analyzerLiterals =
    "Analyzer: literals"
        ~: TestList
            [ testAnalyze "42" (ELit (LNum 42))
            , testAnalyze "#t" (ELit (LBool True))
            , testAnalyze "#f" (ELit (LBool False))
            , testAnalyze "\"hello\"" (ELit (LStr "hello"))
            , testAnalyze "()" (ELit LNil)
            ]

analyzerVariables :: Test
analyzerVariables =
    "Analyzer: variables"
        ~: TestList
            [ testAnalyze "x" (EVar (id' "x"))
            , testAnalyze "+" (EVar (id' "+"))
            , testAnalyze "null?" (EVar (id' "null?"))
            ]

analyzerLambda :: Test
analyzerLambda =
    "Analyzer: lambda"
        ~: TestList
            [ -- (lambda (x) x)
              testAnalyze "(lambda (x) x)" $
                ELambda (Params [id' "x"] Nothing) (Body [] (EVar (id' "x") :| []))
            , -- (lambda (x y) (+ x y))
              testAnalyze "(lambda (x y) (+ x y))" $
                ELambda
                    (Params [id' "x", id' "y"] Nothing)
                    (Body [] (EApp (EVar (id' "+")) [EVar (id' "x"), EVar (id' "y")] :| []))
            , -- (lambda args args) — rest-only
              testAnalyze "(lambda args args)" $
                ELambda (Params [] (Just (id' "args"))) (Body [] (EVar (id' "args") :| []))
            , -- (lambda (x . rest) rest) — fixed + rest
              testAnalyze "(lambda (x . rest) rest)" $
                ELambda (Params [id' "x"] (Just (id' "rest"))) (Body [] (EVar (id' "rest") :| []))
            , -- lambda with no body
              testAnalyzeFail "(lambda (x))"
            , -- duplicate parameter names
              testAnalyzeFail "(lambda (x x) x)"
            , testAnalyzeFail "(lambda (x y x) x)"
            , testAnalyzeFail "(lambda (x . x) x)"
            ]

analyzerQuote :: Test
analyzerQuote =
    "Analyzer: quote"
        ~: TestList
            [ testAnalyze "'x" (EQuote (SSym "x"))
            , testAnalyze "'(1 2 3)" (EQuote (slist [SNum 1, SNum 2, SNum 3]))
            , testAnalyze "(quote x)" (EQuote (SSym "x"))
            , testAnalyzeFail "(quote)"
            , testAnalyzeFail "(quote a b)"
            ]

analyzerSet :: Test
analyzerSet =
    "Analyzer: set!"
        ~: TestList
            [ testAnalyze "(set! x 1)" (ESet (id' "x") (ELit (LNum 1)))
            , testAnalyzeFail "(set! 1 2)"
            , testAnalyzeFail "(set! x)"
            ]

analyzerLet :: Test
analyzerLet =
    "Analyzer: let"
        ~: TestList
            [ -- (let ((x 1)) x)
              testAnalyze "(let ((x 1)) x)" $
                ELet Nothing [(id' "x", ELit (LNum 1))] (Body [] (EVar (id' "x") :| []))
            , -- (let ((x 1) (y 2)) (+ x y))
              testAnalyze "(let ((x 1) (y 2)) (+ x y))" $
                ELet
                    Nothing
                    [(id' "x", ELit (LNum 1)), (id' "y", ELit (LNum 2))]
                    (Body [] (EApp (EVar (id' "+")) [EVar (id' "x"), EVar (id' "y")] :| []))
            , -- Named let: (let loop ((n 0)) n)
              testAnalyze "(let loop ((n 0)) n)" $
                ELet (Just (id' "loop")) [(id' "n", ELit (LNum 0))] (Body [] (EVar (id' "n") :| []))
            , testAnalyzeFail "(let)"
            , testAnalyzeFail "(let ((x 1)))" -- no body
            , testAnalyzeFail "(let ((x 1) (x 2)) x)" -- duplicate binding name
            ]

analyzerLetStar :: Test
analyzerLetStar =
    "Analyzer: let*"
        ~: TestList
            [ testAnalyze "(let* ((x 1) (y x)) y)" $
                ELetStar
                    [(id' "x", ELit (LNum 1)), (id' "y", EVar (id' "x"))]
                    (Body [] (EVar (id' "y") :| []))
            , -- let* permits duplicate binding names (later shadows earlier)
              testAnalyze "(let* ((x 1) (x 2)) x)" $
                ELetStar
                    [(id' "x", ELit (LNum 1)), (id' "x", ELit (LNum 2))]
                    (Body [] (EVar (id' "x") :| []))
            , testAnalyzeFail "(let*)"
            ]

analyzerLetrec :: Test
analyzerLetrec =
    "Analyzer: letrec"
        ~: TestList
            [ testAnalyze "(letrec ((x 1)) x)" $
                ELetrec [(id' "x", ELit (LNum 1))] (Body [] (EVar (id' "x") :| []))
            , testAnalyzeFail "(letrec)"
            , testAnalyzeFail "(letrec ((x 1) (x 2)) x)" -- duplicate binding name
            ]

analyzerIf :: Test
analyzerIf =
    "Analyzer: if"
        ~: TestList
            [ testAnalyze "(if #t 1 2)" $
                EIf (ELit (LBool True)) (ELit (LNum 1)) (Just (ELit (LNum 2)))
            , testAnalyze "(if #t 1)" $
                EIf (ELit (LBool True)) (ELit (LNum 1)) Nothing
            , testAnalyzeFail "(if)"
            , testAnalyzeFail "(if #t)"
            , testAnalyzeFail "(if #t 1 2 3)"
            ]

analyzerCond :: Test
analyzerCond =
    "Analyzer: cond"
        ~: TestList
            [ -- (cond (#t 1))
              testAnalyze "(cond (#t 1))" $
                ECond (CondClauses ((ELit (LBool True), ELit (LNum 1) :| []) :| []) Nothing)
            , -- (cond (#f 1) (else 2))
              testAnalyze "(cond (#f 1) (else 2))" $
                ECond (CondClauses ((ELit (LBool False), ELit (LNum 1) :| []) :| []) (Just (ELit (LNum 2) :| [])))
            , -- (cond (else 42))
              testAnalyze "(cond (else 42))" $
                ECond (CondElseOnly (ELit (LNum 42) :| []))
            , -- cond with multiple body exprs
              testAnalyze "(cond (#t 1 2 3))" $
                ECond (CondClauses ((ELit (LBool True), ELit (LNum 1) :| [ELit (LNum 2), ELit (LNum 3)]) :| []) Nothing)
            , testAnalyzeFail "(cond)"
            , testAnalyzeFail "(cond (#t))" -- clause needs body
            ]

analyzerAndOrBegin :: Test
analyzerAndOrBegin =
    "Analyzer: and, or, begin"
        ~: TestList
            [ testAnalyze "(and)" (EAnd [])
            , testAnalyze "(and 1 2)" (EAnd [ELit (LNum 1), ELit (LNum 2)])
            , testAnalyze "(or)" (EOr [])
            , testAnalyze "(or 1 2)" (EOr [ELit (LNum 1), ELit (LNum 2)])
            , testAnalyze "(begin)" (EBegin [])
            , testAnalyze "(begin 1 2)" (EBegin [ELit (LNum 1), ELit (LNum 2)])
            ]

analyzerDo :: Test
analyzerDo =
    "Analyzer: do"
        ~: TestList
            [ -- (do ((i 0 (+ i 1))) ((= i 5) i) (display i))
              testAnalyze "(do ((i 0 (+ i 1))) ((= i 5) i) (display i))" $
                EDo
                    [(id' "i", ELit (LNum 0), EApp (EVar (id' "+")) [EVar (id' "i"), ELit (LNum 1)])]
                    (EApp (EVar (id' "=")) [EVar (id' "i"), ELit (LNum 5)], [EVar (id' "i")])
                    (Body [] (EApp (EVar (id' "display")) [EVar (id' "i")] :| []))
            , -- do with no body is invalid (Body requires Exp+)
              testAnalyzeFail "(do ((i 0 (+ i 1))) ((= i 5) i))"
            , testAnalyzeFail "(do)"
            , -- duplicate do binding variable
              testAnalyzeFail "(do ((i 0 0) (i 1 1)) ((= i 0) i) i)"
            ]

analyzerApp :: Test
analyzerApp =
    "Analyzer: function application"
        ~: TestList
            [ testAnalyze "(f)" (EApp (EVar (id' "f")) [])
            , testAnalyze "(f 1 2)" (EApp (EVar (id' "f")) [ELit (LNum 1), ELit (LNum 2)])
            , testAnalyze "((lambda (x) x) 1)" $
                EApp
                    (ELambda (Params [id' "x"] Nothing) (Body [] (EVar (id' "x") :| [])))
                    [ELit (LNum 1)]
            ]

analyzerDefine :: Test
analyzerDefine =
    "Analyzer: define (toplevel)"
        ~: TestList
            [ -- (define x 42)
              testAnalyzeTL "(define x 42)" $
                TDefine (Define (id' "x") (ELit (LNum 42)))
            , -- (define (f x) x) => (define f (lambda (x) x))
              testAnalyzeTL "(define (f x) x)" $
                TDefine (Define (id' "f") (ELambda (Params [id' "x"] Nothing) (Body [] (EVar (id' "x") :| []))))
            , -- (define (f x . rest) rest)
              testAnalyzeTL "(define (f x . rest) rest)" $
                TDefine (Define (id' "f") (ELambda (Params [id' "x"] (Just (id' "rest"))) (Body [] (EVar (id' "rest") :| []))))
            , testAnalyzeTLFail "(define)"
            , testAnalyzeTLFail "(define 1 2)"
            , -- duplicate parameter names in function sugar
              testAnalyzeTLFail "(define (f x x) x)"
            , -- define function sugar without body
              testAnalyzeTLFail "(define (f x))"
            ]

analyzerLoad :: Test
analyzerLoad =
    "Analyzer: load"
        ~: TestList
            [ testAnalyzeTL "(load \"file.scm\")" (TLoad "file.scm")
            , testAnalyzeTLFail "(load 42)"
            , testAnalyzeTLFail "(load)"
            ]

analyzerBody :: Test
analyzerBody =
    "Analyzer: body with internal defines"
        ~: TestList
            [ -- (lambda () (define x 1) x)
              testAnalyze "(lambda () (define x 1) x)" $
                ELambda
                    (Params [] Nothing)
                    (Body [Define (id' "x") (ELit (LNum 1))] (EVar (id' "x") :| []))
            , -- (lambda () (define a 1) (define b 2) (+ a b))
              testAnalyze "(lambda () (define a 1) (define b 2) (+ a b))" $
                ELambda
                    (Params [] Nothing)
                    ( Body
                        [Define (id' "a") (ELit (LNum 1)), Define (id' "b") (ELit (LNum 2))]
                        (EApp (EVar (id' "+")) [EVar (id' "a"), EVar (id' "b")] :| [])
                    )
            ]

analyzerErrors :: Test
analyzerErrors =
    "Analyzer: error cases"
        ~: TestList
            [ -- define in expression context
              testAnalyzeFail "(+ (define x 1) 2)"
            , -- load in expression context
              testAnalyzeFail "(+ (load \"foo.scm\") 1)"
            , testAnalyzeFail "(load \"foo.scm\")" -- load at expression position
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
                , analyzerLiterals
                , analyzerVariables
                , analyzerLambda
                , analyzerQuote
                , analyzerSet
                , analyzerLet
                , analyzerLetStar
                , analyzerLetrec
                , analyzerIf
                , analyzerCond
                , analyzerAndOrBegin
                , analyzerDo
                , analyzerApp
                , analyzerDefine
                , analyzerLoad
                , analyzerBody
                , analyzerErrors
                , step0
                , step1
                ]
    if errors result + failures result == 0
        then exitSuccess
        else exitFailure
