module SchemeSpecRunner (
    coreSpecSuite,
    floatingNumberSpecSuite,
    optionSpecSyntaxSuite,
    parseErrorSpecSuite,
    r5rsSpecSuite,
    tcoSpecSuite,
) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (foldM)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Scheme.Interpreter qualified as Interpreter
import Scheme.Parser (parseFile, prettyError)
import System.Directory (findExecutable, getTemporaryDirectory, removeFile)
import System.Environment qualified as Environment
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.HUnit (
    Assertion,
    Test (TestCase, TestList),
    assertEqual,
    assertFailure,
    (~:),
 )

data Expected
    = ExpectedOutput Text
    | ExpectedError (Maybe Text)
    deriving stock (Eq, Show)

data SpecForm = SpecForm
    { specPath :: FilePath
    , specLine :: Int
    , specSource :: Text
    , specExpected :: Maybe Expected
    }
    deriving stock (Eq, Show)

data ScanState = ScanState
    { scanForms :: [SpecForm]
    , scanLines :: [Text]
    , scanStartLine :: Maybe Int
    , scanExpected :: Maybe Expected
    , scanDepth :: Int
    }
    deriving stock (Eq, Show)

data R5RSCommand = R5RSCommand
    { r5rsName :: Text
    , r5rsExecutable :: FilePath
    , r5rsArgsBeforeScript :: [String]
    }

coreSpecSuite :: Test
coreSpecSuite =
    "Mini-Scheme executable specs"
        ~: TestList (specFileTest <$> coreSpecFiles)

optionSpecSyntaxSuite :: Test
optionSpecSyntaxSuite =
    "Mini-Scheme option spec syntax"
        ~: TestList (parseableSpecFileTest <$> optionSpecFiles)

parseErrorSpecSuite :: Test
parseErrorSpecSuite =
    "Mini-Scheme parse error specs"
        ~: TestList (parseErrorSpecTest <$> parseErrorSpecFiles)

r5rsSpecSuite :: Test
r5rsSpecSuite =
    "R5RS-portable specs"
        ~: TestList
            [ "Mini-Scheme"
                ~: TestList (specFileTest <$> r5rsPortableSpecFiles)
            , "R5RS differential"
                ~: TestCase runR5RSDifferential
            ]

tcoSpecSuite :: Test
tcoSpecSuite =
    "Mini-Scheme TCO specs"
        ~: TestList (specFileTest <$> tcoSpecFiles)

floatingNumberSpecSuite :: Test
floatingNumberSpecSuite =
    "Mini-Scheme floating-number specs"
        ~: TestList (specFileTest <$> floatingNumberSpecFiles)

coreSpecFiles :: [FilePath]
coreSpecFiles =
    [ "test/spec/core/step0-parse-literals.scm"
    , "test/spec/core/step1-arithmetic.scm"
    , "test/spec/core/step2-define-lambda.scm"
    , "test/spec/core/step3-special-forms.scm"
    , "test/spec/core/step4-let-letrec-do.scm"
    , "test/spec/core/step5-list-ops.scm"
    , "test/spec/core/step6-string-eq-misc.scm"
    , "test/spec/core/step7-errors.scm"
    , "test/spec/core/step10-integration.scm"
    , "test/spec/core/step11-target-regressions.scm"
    ]

optionSpecFiles :: [FilePath]
optionSpecFiles =
    [ "test/spec/options/step8-tco.scm"
    , "test/spec/options/floating-numbers.scm"
    , "test/spec/options/step9-macros.scm"
    , "test/spec/options/step11-next-target-programs.scm"
    , "test/spec/options/manual-stress-programs.scm"
    ]

tcoSpecFiles :: [FilePath]
tcoSpecFiles =
    [ "test/spec/options/step8-tco.scm"
    ]

floatingNumberSpecFiles :: [FilePath]
floatingNumberSpecFiles =
    [ "test/spec/options/floating-numbers.scm"
    ]

parseErrorSpecFiles :: [FilePath]
parseErrorSpecFiles =
    [ "test/spec/errors/parse-unexpected-eof.scm"
    , "test/spec/errors/parse-unexpected-token.scm"
    , "test/spec/errors/parse-unterminated-string.scm"
    ]

r5rsPortableSpecFiles :: [FilePath]
r5rsPortableSpecFiles =
    [ "test/spec/r5rs/portable-core.scm"
    ]

specFileTest :: FilePath -> Test
specFileTest path =
    path ~: TestCase (runSpecFile path)

parseableSpecFileTest :: FilePath -> Test
parseableSpecFileTest path =
    path ~: TestCase $ do
        input <- TIO.readFile path
        case parseFile path input of
            Left err ->
                assertFailure $
                    path <> ": expected parseable option spec, got " <> prettyError err
            Right _ -> pure ()

parseErrorSpecTest :: FilePath -> Test
parseErrorSpecTest path =
    path ~: TestCase $ do
        input <- TIO.readFile path
        case parseFile path input of
            Left _ -> pure ()
            Right actual ->
                assertFailure $
                    path <> ": expected parse error, got " <> show actual

runSpecFile :: FilePath -> Assertion
runSpecFile path = do
    forms <- readSpecForms path
    env <- Interpreter.initialEnv
    runMiniForms env forms

readSpecForms :: FilePath -> IO [SpecForm]
readSpecForms path = do
    input <- TIO.readFile path
    case parseSpecForms path input of
        Left err -> assertFailure (T.unpack err)
        Right forms -> pure forms

parseSpecForms :: FilePath -> Text -> Either Text [SpecForm]
parseSpecForms path input =
    finishScan
        =<< foldM
            (scanLine path)
            (ScanState [] [] Nothing Nothing 0)
            (zip [1 ..] (T.lines input))

finishScan :: ScanState -> Either Text [SpecForm]
finishScan scan = case scanStartLine scan of
    Nothing -> Right (reverse (scanForms scan))
    Just line ->
        Left $
            "unterminated top-level form starting at "
                <> T.pack (show line)

scanLine :: FilePath -> ScanState -> (Int, Text) -> Either Text ScanState
scanLine path scan (lineNumber, rawLine)
    | not hasCode && isNothing (scanStartLine scan) = Right scan
    | not hasCode = Right scan
    | otherwise = do
        expected <- mergeExpected path lineNumber (scanExpected scan) lineExpected
        let startLine = scanStartLine scan <|> Just lineNumber
            linesSoFar = scanLines scan <> [code]
            depth = scanDepth scan + parenDelta code
        if depth < 0
            then
                Left $
                    location path lineNumber
                        <> ": too many closing parentheses"
            else
                if depth == 0
                    then
                        Right $
                            ScanState
                                { scanForms =
                                    SpecForm
                                        { specPath = path
                                        , specLine = fromMaybe lineNumber startLine
                                        , specSource = T.intercalate "\n" linesSoFar
                                        , specExpected = expected
                                        }
                                        : scanForms scan
                                , scanLines = []
                                , scanStartLine = Nothing
                                , scanExpected = Nothing
                                , scanDepth = 0
                                }
                    else
                        Right $
                            scan
                                { scanLines = linesSoFar
                                , scanStartLine = startLine
                                , scanExpected = expected
                                , scanDepth = depth
                                }
  where
    (codePart, commentPart) = splitCodeComment rawLine
    code = T.stripEnd codePart
    hasCode = not (T.null (T.strip code))
    lineExpected = parseExpectation commentPart

mergeExpected ::
    FilePath ->
    Int ->
    Maybe Expected ->
    Maybe Expected ->
    Either Text (Maybe Expected)
mergeExpected path lineNumber current new = case (current, new) of
    (Just _, Just _) ->
        Left $
            location path lineNumber
                <> ": multiple expectations attached to one form"
    (_, Just expected) -> Right (Just expected)
    _ -> Right current

splitCodeComment :: Text -> (Text, Text)
splitCodeComment line =
    let (code, comment) = go False False [] (T.unpack line)
     in (T.pack (reverse code), T.pack comment)
  where
    go :: Bool -> Bool -> [Char] -> [Char] -> ([Char], [Char])
    go _ _ acc [] = (acc, [])
    go inString escaped acc input@(c : rest)
        | not inString && c == ';' = (acc, input)
        | inString && not escaped && c == '\\' = go True True (c : acc) rest
        | inString && escaped = go True False (c : acc) rest
        | c == '"' = go (not inString) False (c : acc) rest
        | otherwise = go inString False (c : acc) rest

parenDelta :: Text -> Int
parenDelta line = go False False 0 (T.unpack line)
  where
    go :: Bool -> Bool -> Int -> [Char] -> Int
    go _ _ depth [] = depth
    go inString escaped depth (c : rest)
        | inString && not escaped && c == '\\' = go True True depth rest
        | inString && escaped = go True False depth rest
        | c == '"' = go (not inString) False depth rest
        | inString = go True False depth rest
        | c == '(' = go False False (depth + 1) rest
        | c == ')' = go False False (depth - 1) rest
        | otherwise = go False False depth rest

parseExpectation :: Text -> Maybe Expected
parseExpectation comment =
    let (_, markerAndExpected) = T.breakOn "=>" comment
     in if T.null markerAndExpected
            then Nothing
            else
                let expectedText = T.strip (T.drop 2 markerAndExpected)
                 in Just (parseExpectedText expectedText)

parseExpectedText :: Text -> Expected
parseExpectedText expected
    | "ERROR contains" `T.isPrefixOf` expected =
        let needle =
                stripOptionalQuotes $
                    T.strip (T.drop (T.length ("ERROR contains" :: Text)) expected)
         in ExpectedError (nonEmptyText needle)
    | "ERROR" `T.isPrefixOf` expected = ExpectedError Nothing
    | otherwise = ExpectedOutput expected

stripOptionalQuotes :: Text -> Text
stripOptionalQuotes input =
    fromMaybe input $ do
        withoutPrefix <- T.stripPrefix "\"" input
        T.stripSuffix "\"" withoutPrefix

nonEmptyText :: Text -> Maybe Text
nonEmptyText input
    | T.null input = Nothing
    | otherwise = Just input

runMiniForms :: Interpreter.Env -> [SpecForm] -> Assertion
runMiniForms env = mapM_ (runMiniForm env)

runMiniForm :: Interpreter.Env -> SpecForm -> Assertion
runMiniForm env form = do
    result <- Interpreter.runIn env (specSource form)
    case specExpected form of
        Nothing -> assertSetupSucceeded form result
        Just (ExpectedOutput expected) -> assertOutput form expected result
        Just (ExpectedError expectedNeedle) -> assertError form expectedNeedle result

assertSetupSucceeded :: SpecForm -> Either Text Text -> Assertion
assertSetupSucceeded form result = case result of
    Right _ -> pure ()
    Left err ->
        assertFailure $
            T.unpack (formLocation form)
                <> ": setup form failed: "
                <> T.unpack err

assertOutput :: SpecForm -> Text -> Either Text Text -> Assertion
assertOutput form expected result = case result of
    Right actual ->
        assertEqual
            (T.unpack (formLocation form))
            expected
            actual
    Left err ->
        assertFailure $
            T.unpack (formLocation form)
                <> ": expected "
                <> T.unpack expected
                <> ", got error: "
                <> T.unpack err

assertError :: SpecForm -> Maybe Text -> Either Text Text -> Assertion
assertError form expectedNeedle result = case result of
    Left err -> case expectedNeedle of
        Nothing -> pure ()
        Just needle
            | needle `T.isInfixOf` err -> pure ()
            | otherwise ->
                assertFailure $
                    T.unpack (formLocation form)
                        <> ": expected error containing "
                        <> T.unpack needle
                        <> ", got "
                        <> T.unpack err
    Right actual ->
        assertFailure $
            T.unpack (formLocation form)
                <> ": expected error, got "
                <> T.unpack actual

formLocation :: SpecForm -> Text
formLocation form = location (specPath form) (specLine form)

location :: FilePath -> Int -> Text
location path lineNumber =
    toText path <> ":" <> T.pack (show lineNumber)

runR5RSDifferential :: Assertion
runR5RSDifferential = do
    command <- findR5RSCommand
    case command of
        Nothing -> pure ()
        Just r5rs -> do
            forms <- concat <$> mapM readSpecForms r5rsPortableSpecFiles
            miniOutputs <- runMiniAndCollectOutputs forms
            r5rsOutputs <- runR5RSAndCollectOutputs r5rs forms
            assertEqual
                ("R5RS command: " <> T.unpack (r5rsName r5rs))
                miniOutputs
                r5rsOutputs

runMiniAndCollectOutputs :: [SpecForm] -> IO [Text]
runMiniAndCollectOutputs forms = do
    env <- Interpreter.initialEnv
    outputs <-
        forM forms $ \form -> do
            result <- Interpreter.runIn env (specSource form)
            case specExpected form of
                Nothing -> do
                    assertSetupSucceeded form result
                    pure Nothing
                Just (ExpectedOutput expected) -> do
                    assertOutput form expected result
                    case result of
                        Right actual -> pure (Just actual)
                        Left _ -> pure Nothing
                Just (ExpectedError _) ->
                    assertFailure $
                        T.unpack (formLocation form)
                            <> ": R5RS differential specs cannot expect errors"
    pure (catMaybes outputs)

runR5RSAndCollectOutputs :: R5RSCommand -> [SpecForm] -> IO [Text]
runR5RSAndCollectOutputs command forms = do
    let script = renderR5RSScript forms
    withTempScript script $ \scriptPath -> do
        let exe = r5rsExecutable command
            args = r5rsArgsBeforeScript command <> [scriptPath]
        result <-
            try (readProcessWithExitCode exe args "") ::
                IO (Either IOException (ExitCode, String, String))
        case result of
            Left err ->
                assertFailure $
                    "failed to run R5RS command "
                        <> T.unpack (r5rsName command)
                        <> ": "
                        <> show err
            Right (ExitSuccess, stdoutText, _) ->
                pure (T.lines (T.stripEnd (T.pack stdoutText)))
            Right (ExitFailure code, stdoutText, stderrText) ->
                assertFailure $
                    "R5RS command "
                        <> T.unpack (r5rsName command)
                        <> " exited with "
                        <> show code
                        <> "\nstdout:\n"
                        <> stdoutText
                        <> "\nstderr:\n"
                        <> stderrText

withTempScript :: Text -> (FilePath -> IO a) -> IO a
withTempScript script =
    bracket createTempScript removeTempScript
  where
    createTempScript :: IO FilePath
    createTempScript = do
        tmpDir <- getTemporaryDirectory
        (path, handle) <- openTempFile tmpDir "mini-scheme-r5rs-differential.scm"
        TIO.hPutStr handle script
        hClose handle
        pure path

removeTempScript :: FilePath -> IO ()
removeTempScript path = do
    result <- try (removeFile path) :: IO (Either IOException ())
    case result of
        Left _ -> pure ()
        Right () -> pure ()

renderR5RSScript :: [SpecForm] -> Text
renderR5RSScript forms =
    T.unlines $
        [ "(define (mini-scheme-spec-print value)"
        , "  (write value)"
        , "  (newline))"
        ]
            <> concatMap renderR5RSForm forms

renderR5RSForm :: SpecForm -> [Text]
renderR5RSForm form = case specExpected form of
    Nothing -> [specSource form]
    Just (ExpectedOutput _) ->
        [ "(mini-scheme-spec-print"
        , specSource form
        , ")"
        ]
    Just (ExpectedError _) -> []

findR5RSCommand :: IO (Maybe R5RSCommand)
findR5RSCommand = do
    configured <- Environment.lookupEnv "MINI_SCHEME_R5RS"
    case configured of
        Just executable ->
            pure $
                Just
                    R5RSCommand
                        { r5rsName = "MINI_SCHEME_R5RS=" <> toText executable
                        , r5rsExecutable = executable
                        , r5rsArgsBeforeScript = []
                        }
        Nothing -> firstAvailableR5RS knownR5RSCommands

knownR5RSCommands :: [(String, [String])]
knownR5RSCommands =
    [ ("guile", [])
    , ("racket", ["-I", "r5rs"])
    ]

firstAvailableR5RS :: [(String, [String])] -> IO (Maybe R5RSCommand)
firstAvailableR5RS [] = pure Nothing
firstAvailableR5RS ((name, args) : rest) = do
    found <- findExecutable name
    case found of
        Nothing -> firstAvailableR5RS rest
        Just executable ->
            pure $
                Just
                    R5RSCommand
                        { r5rsName = toText name
                        , r5rsExecutable = executable
                        , r5rsArgsBeforeScript = args
                        }
