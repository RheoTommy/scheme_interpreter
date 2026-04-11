module Main (main) where

import Data.Text qualified as T
import Sample.Interpreter (run)
import Test.HUnit
  ( Counts (errors, failures),
    Test (TestList),
    assertEqual,
    assertFailure,
    runTestTT,
    (~:),
  )

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

-- === Step 0: Literals ===
step0 :: Test
step0 =
  "Step 0: Literals"
    ~: TestList
      [ testEval "42" "42",
        testEval "0" "0",
        testEval "-1" "-1"
      ]

-- === Step 1: Arithmetic ===
step1 :: Test
step1 =
  "Step 1: Arithmetic"
    ~: TestList
      [ -- Addition
        testEval "(+ 1 2)" "3",
        testEval "(+ 1 2 3)" "6",
        testEval "(+)" "0",
        -- Subtraction
        testEval "(- 5 3)" "2",
        testEval "(- 10 3 2)" "5",
        testEval "(- 5)" "-5",
        -- Multiplication
        testEval "(* 2 3)" "6",
        testEval "(* 2 3 4)" "24",
        testEval "(*)" "1",
        -- Division
        testEval "(/ 10 2)" "5",
        testEval "(/ 10 3)" "3",
        -- Nested
        testEval "(+ 1 (* 2 3))" "7",
        testEval "(* (+ 1 2) (+ 3 4))" "21",
        testEval "(- (* 3 3) (* 2 2))" "5",
        -- Errors
        testError "(/ 10 0)",
        testError "(-)",
        testError "(/ 1)",
        testError "(+ 1 x)"
      ]

main :: IO ()
main = do
  result <- runTestTT $ TestList [step0, step1]
  if errors result + failures result == 0
    then exitSuccess
    else exitFailure
