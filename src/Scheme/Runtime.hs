{- | Runtime types and evaluation monad.

Contains mutually recursive runtime types in a single module:

  * 'Value'  — runtime values (numbers, booleans, closures, builtins, ...)
  * 'Frame' / 'Env' — lexical environment
  * 'Eval'   — evaluation monad: @ExceptT EvalError (ReaderT Env IO)@

These must live together because 'Value' carries 'Eval' (in 'VBuiltin')
and 'VClosure', while 'Eval' carries 'Env' which carries 'Value'.
-}
module Scheme.Runtime (
    -- * Values
    Value (VNum, VBool, VStr, VSym, VNil, VUnspecified, VPair, VClosure, VBuiltin),
    showValue,
    showValueIO,
    showValueKind,
    atomicValueEq,
    PairCell,
    pairSeen,
    samePair,

    -- * Environment
    Env,
    Frame,
    newFrame,
    newChildFrame,
    bindInFrame,
    bindUninitializedInFrame,
    defineVar,
    defineUninitializedVar,
    lookupVar,
    setVar,

    -- * Errors
    EvalError (
        UnboundVariable,
        TypeError,
        ArityError,
        NotAProcedure,
        DivisionByZero,
        OtherEvalError
    ),
    prettyEvalError,

    -- * Monad
    Eval,
    runEval,
) where

import Control.Monad.Except (throwError)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Scheme.AST (Body, Id, Params, idToText)

-- * Value

{- | Runtime value.

Mutually recursive with 'Env' via 'VClosure' and with 'Eval' via 'VBuiltin'.
-}
data Value
    = VNum Integer
    | VBool Bool
    | VStr Text
    | VSym Text
    | VNil
    | -- | The result of forms whose value is intentionally unspecified.
      VUnspecified
    | -- | Mutable pair (enables @set-car!@ and @set-cdr!@).
      VPair (IORef Value) (IORef Value)
    | -- | Closure: formal params, body, captured environment.
      VClosure Params Body Env
    | -- | Builtin procedure. The name is retained for display and error messages.
      VBuiltin Text ([Value] -> Eval Value)
    | -- | Placeholder used while evaluating recursive bindings.
      VUninitialized Text

{- | Approximate display of a value. Does not traverse pairs (that would
require 'IO'); a richer printer can be layered on top.
-}
showValue :: Value -> Text
showValue v = case v of
    VNum n -> show n
    VBool True -> "#t"
    VBool False -> "#f"
    VStr s -> "\"" <> escapeSchemeString s <> "\""
    VSym s -> s
    VNil -> "()"
    VUnspecified -> "(unspecified)"
    VPair{} -> "<pair>"
    VClosure{} -> "<closure>"
    VBuiltin name _ -> "<builtin: " <> name <> ">"
    VUninitialized name -> "<uninitialized: " <> name <> ">"

-- | Display a value, traversing pairs in 'IO'.
showValueIO :: Value -> IO Text
showValueIO = showValueWith []
  where
    showValueWith :: [PairCell] -> Value -> IO Text
    showValueWith seen v = case v of
        VPair carRef cdrRef ->
            let pair = (carRef, cdrRef)
             in if pairSeen pair seen
                    then pure "#<cycle>"
                    else showPair (pair : seen) carRef cdrRef
        _ -> pure (showValue v)

    showPair :: [PairCell] -> IORef Value -> IORef Value -> IO Text
    showPair seen carRef cdrRef = do
        car <- readIORef carRef
        cdr <- readIORef cdrRef
        carText <- showValueWith seen car
        cdrText <- showPairTail seen cdr
        pure $ "(" <> carText <> cdrText <> ")"

    showPairTail :: [PairCell] -> Value -> IO Text
    showPairTail _ VNil = pure ""
    showPairTail seen (VPair carRef cdrRef) =
        let pair = (carRef, cdrRef)
         in if pairSeen pair seen
                then pure " . #<cycle>"
                else showPairTailPair (pair : seen) carRef cdrRef
    showPairTail seen other = do
        otherText <- showValueWith seen other
        pure $ " . " <> otherText

    showPairTailPair :: [PairCell] -> IORef Value -> IORef Value -> IO Text
    showPairTailPair seen carRef cdrRef = do
        car <- readIORef carRef
        cdr <- readIORef cdrRef
        carText <- showValueWith seen car
        cdrText <- showPairTail seen cdr
        pure $ " " <> carText <> cdrText

type PairCell = (IORef Value, IORef Value)

pairSeen :: PairCell -> [PairCell] -> Bool
pairSeen pair = any (samePair pair)

samePair :: PairCell -> PairCell -> Bool
samePair (carA, cdrA) (carB, cdrB) = carA == carB && cdrA == cdrB

-- | Escape Scheme string metacharacters for round-trippable output.
escapeSchemeString :: Text -> Text
escapeSchemeString = T.concatMap escape
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\t' = "\\t"
    escape '\r' = "\\r"
    escape c = T.singleton c

{- | Shallow structural equality on atomic values.

Compares 'VNum', 'VBool', 'VStr', 'VSym', 'VNil' by contents. Treats
pairs, closures, and builtins as always unequal — they need 'IO' for
proper structural equality or reference equality, which do not fit a
pure function. Intended for test assertions on atomic values; runtime
@eq?@ / @equal?@ will be implemented separately.
-}
atomicValueEq :: Value -> Value -> Bool
atomicValueEq (VNum a) (VNum b) = a == b
atomicValueEq (VBool a) (VBool b) = a == b
atomicValueEq (VStr a) (VStr b) = a == b
atomicValueEq (VSym a) (VSym b) = a == b
atomicValueEq VNil VNil = True
atomicValueEq VUnspecified VUnspecified = True
atomicValueEq _ _ = False

-- | Describe the "kind" of a value, for use in error messages.
showValueKind :: Value -> Text
showValueKind v = case v of
    VNum _ -> "number"
    VBool _ -> "boolean"
    VStr _ -> "string"
    VSym _ -> "symbol"
    VNil -> "nil"
    VUnspecified -> "unspecified"
    VPair{} -> "pair"
    VClosure{} -> "closure"
    VBuiltin{} -> "builtin"
    VUninitialized{} -> "uninitialized"

-- * Environment

{- | A lexical frame.

The outer 'IORef' allows adding new bindings to an existing frame
(needed for @define@). Each variable has its own inner 'IORef' so that
@set!@ performs in-place mutation that is shared across closures which
captured the same variable.
-}
data Frame = Frame
    { parent :: Maybe Frame
    , bindings :: IORef (Map Id (IORef Value))
    }

-- | Current lexical environment. Same as 'Frame'; named separately for clarity.
type Env = Frame

-- | Create a new top-level frame with no parent.
newFrame :: IO Frame
newFrame = do
    ref <- newIORef Map.empty
    pure $ Frame{parent = Nothing, bindings = ref}

-- | Create a child frame with the given lexical parent.
newChildFrame :: Frame -> IO Frame
newChildFrame parentFrame = do
    ref <- newIORef Map.empty
    pure $ Frame{parent = Just parentFrame, bindings = ref}

-- | Insert or overwrite a binding in the given frame.
bindInFrame :: Id -> Value -> Frame -> IO ()
bindInFrame ident val frame = do
    ref <- newIORef val
    modifyIORef' (bindings frame) (Map.insert ident ref)

-- | Insert an uninitialized binding in the given frame.
bindUninitializedInFrame :: Id -> Frame -> IO ()
bindUninitializedInFrame ident =
    bindInFrame ident (VUninitialized (idToText ident))

-- | Define or overwrite a binding in the current frame.
defineVar :: Id -> Value -> Eval ()
defineVar ident val = do
    env <- ask
    liftIO $ bindInFrame ident val env

-- | Define or overwrite an uninitialized binding in the current frame.
defineUninitializedVar :: Id -> Eval ()
defineUninitializedVar ident = do
    env <- ask
    liftIO $ bindUninitializedInFrame ident env

-- | Look up a variable, walking up the parent chain.
lookupVar :: Id -> Eval Value
lookupVar ident = do
    env <- ask
    mref <- liftIO (findRef ident env)
    case mref of
        Just ref -> do
            value <- readIORef ref
            case value of
                VUninitialized name -> throwError $ OtherEvalError ("uninitialized variable: " <> name)
                _ -> pure value
        Nothing -> throwError $ UnboundVariable (idToText ident)

-- | Update an existing binding, walking up the parent chain.
setVar :: Id -> Value -> Eval ()
setVar ident val = do
    env <- ask
    mref <- liftIO (findRef ident env)
    case mref of
        Just ref -> liftIO $ writeIORef ref val
        Nothing -> throwError $ UnboundVariable (idToText ident)

findRef :: Id -> Frame -> IO (Maybe (IORef Value))
findRef ident frame = do
    m <- readIORef (bindings frame)
    case Map.lookup ident m of
        Just r -> pure (Just r)
        Nothing -> case parent frame of
            Just p -> findRef ident p
            Nothing -> pure Nothing

-- * Errors

-- | Runtime evaluation errors.
data EvalError
    = UnboundVariable Text
    | TypeError Text
    | ArityError Text
    | NotAProcedure Text
    | DivisionByZero
    | OtherEvalError Text
    deriving stock (Show, Eq)

-- | Format an 'EvalError' as a human-readable single-line message.
prettyEvalError :: EvalError -> Text
prettyEvalError err = case err of
    UnboundVariable name -> "unbound variable: " <> name
    TypeError msg -> "type error: " <> msg
    ArityError msg -> "arity error: " <> msg
    NotAProcedure kind -> "not a procedure: " <> kind
    DivisionByZero -> "division by zero"
    OtherEvalError msg -> msg

-- * Eval monad

{- | Evaluation monad: errors on top, current environment in the middle,
'IO' at the base for mutable cells.
-}
type Eval a = ExceptT EvalError (ReaderT Env IO) a

-- | Run an 'Eval' computation in the given environment.
runEval :: Env -> Eval a -> IO (Either EvalError a)
runEval env m = runReaderT (runExceptT m) env
