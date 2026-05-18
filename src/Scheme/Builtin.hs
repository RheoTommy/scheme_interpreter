{- | Builtin procedures.

Exports the initial list of builtin bindings that populate the top-level
environment.

This module provides the small builtin surface needed by the current
Mini-Scheme evaluator: numeric arithmetic, predicates, mutable pairs,
proper-list utilities, equality, and basic string/symbol conversions.
-}
module Scheme.Builtin (builtins) where

import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Scheme.Number (Number, numberDiv, numberToText, parseNumberText)
import Scheme.Runtime (
    Eval,
    EvalError (ArityError, DivisionByZero, OtherEvalError, TypeError),
    PairCell,
    Value (VBool, VBuiltin, VClosure, VContinuation, VNil, VNum, VPair, VStr, VSym, VUnspecified),
    pairSeen,
    samePair,
    showValueIO,
    showValueKind,
 )

-- | All builtins, keyed by Scheme name after validation in 'Scheme.Environment.initialEnv'.
builtins :: [(Text, Value)]
builtins =
    [ mkBuiltin "+" builtinPlus
    , mkBuiltin "-" builtinMinus
    , mkBuiltin "*" builtinTimes
    , mkBuiltin "/" builtinDiv
    , mkBuiltin "=" (numericCmp "=" (==))
    , mkBuiltin "<" (numericCmp "<" (<))
    , mkBuiltin "<=" (numericCmp "<=" (<=))
    , mkBuiltin ">" (numericCmp ">" (>))
    , mkBuiltin ">=" (numericCmp ">=" (>=))
    , mkBuiltin "number?" builtinNumberP
    , mkBuiltin "boolean?" (unaryPredicate "boolean?" isBool)
    , mkBuiltin "string?" (unaryPredicate "string?" isString)
    , mkBuiltin "symbol?" (unaryPredicate "symbol?" isSymbol)
    , mkBuiltin "null?" (unaryPredicate "null?" isNull)
    , mkBuiltin "pair?" (unaryPredicate "pair?" isPair)
    , mkBuiltin "list?" builtinListP
    , mkBuiltin "procedure?" (unaryPredicate "procedure?" isProcedure)
    , mkBuiltin "not" builtinNot
    , mkBuiltin "call/cc" builtinCallCC
    , mkBuiltin "call-with-current-continuation" builtinCallCC
    , mkBuiltin "cons" builtinCons
    , mkBuiltin "car" builtinCar
    , mkBuiltin "cdr" builtinCdr
    ]
        <> carCdrBuiltins
        <> [ mkBuiltin "list" builtinList
           , mkBuiltin "display" builtinDisplay
           , mkBuiltin "newline" builtinNewline
           , mkBuiltin "print" builtinDisplay
           , mkBuiltin "length" builtinLength
           , mkBuiltin "append" builtinAppend
           , mkBuiltin "last" builtinLast
           , mkBuiltin "memq" builtinMemq
           , mkBuiltin "set-car!" builtinSetCar
           , mkBuiltin "set-cdr!" builtinSetCdr
           , mkBuiltin "string-append" builtinStringAppend
           , mkBuiltin "symbol->string" builtinSymbolToString
           , mkBuiltin "string->symbol" builtinStringToSymbol
           , mkBuiltin "string->number" builtinStringToNumber
           , mkBuiltin "number->string" builtinNumberToString
           , mkBuiltin "eq?" builtinEqP
           , mkBuiltin "neq?" builtinNeqP
           , mkBuiltin "equal?" builtinEqualP
           ]

{- | Build a @(name, VBuiltin name fn)@ pair. Having a single source of
truth for the name prevents the key and the 'VBuiltin' label from drifting.
-}
mkBuiltin :: Text -> ([Value] -> Eval Value) -> (Text, Value)
mkBuiltin name fn = (name, VBuiltin name fn)

arityError :: Text -> Text -> Eval a
arityError name expected =
    throwError $ ArityError (name <> ": expected " <> expected)

typeError :: Text -> Text -> Value -> Eval a
typeError name expected value =
    throwError $ TypeError (name <> ": expected " <> expected <> ", got " <> showValueKind value)

unaryArg :: Text -> [Value] -> Eval Value
unaryArg _ [value] = pure value
unaryArg name _ = arityError name "exactly 1 argument"

binaryArgs :: Text -> [Value] -> Eval (Value, Value)
binaryArgs _ [a, b] = pure (a, b)
binaryArgs name _ = arityError name "exactly 2 arguments"

-- | Require a value to be a number; otherwise raise 'TypeError'.
asNum :: Text -> Value -> Eval Number
asNum _ (VNum n) = pure n
asNum name value = typeError name "number" value

asString :: Text -> Value -> Eval Text
asString _ (VStr value) = pure value
asString name value = typeError name "string" value

asSymbol :: Text -> Value -> Eval Text
asSymbol _ (VSym value) = pure value
asSymbol name value = typeError name "symbol" value

asPair :: Text -> Value -> Eval (IORef Value, IORef Value)
asPair _ (VPair carRef cdrRef) = pure (carRef, cdrRef)
asPair name value = typeError name "pair" value

type PairComparison = (PairCell, PairCell)

comparisonSeen :: PairComparison -> [PairComparison] -> Bool
comparisonSeen pair = any (sameComparison pair)

sameComparison :: PairComparison -> PairComparison -> Bool
sameComparison (leftA, rightA) (leftB, rightB) =
    samePair leftA leftB && samePair rightA rightB

builtinPlus :: [Value] -> Eval Value
builtinPlus args = VNum . sum <$> traverse (asNum "+") args

builtinMinus :: [Value] -> Eval Value
builtinMinus [] = throwError $ ArityError "-: expected at least 1 argument"
builtinMinus [x] = VNum . negate <$> asNum "-" x
builtinMinus (x : xs) = do
    n <- asNum "-" x
    ns <- traverse (asNum "-") xs
    pure $ VNum (foldl' (-) n ns)

builtinTimes :: [Value] -> Eval Value
builtinTimes args = VNum . product <$> traverse (asNum "*") args

builtinDiv :: [Value] -> Eval Value
builtinDiv [a, b] = do
    n <- asNum "/" a
    m <- asNum "/" b
    case numberDiv n m of
        Just q -> pure (VNum q)
        Nothing -> throwError DivisionByZero
builtinDiv _ = throwError $ ArityError "/: expected exactly 2 arguments"

-- | Binary numeric comparison: @(op a b)@ returns a boolean.
numericCmp :: Text -> (Number -> Number -> Bool) -> [Value] -> Eval Value
numericCmp name op [a, b] = do
    n <- asNum name a
    m <- asNum name b
    pure $ VBool (op n m)
numericCmp name _ _ =
    throwError $ ArityError (name <> ": expected exactly 2 arguments")

builtinNumberP :: [Value] -> Eval Value
builtinNumberP [VNum _] = pure (VBool True)
builtinNumberP [_] = pure (VBool False)
builtinNumberP _ =
    throwError $ ArityError "number?: expected exactly 1 argument"

unaryPredicate :: Text -> (Value -> Bool) -> [Value] -> Eval Value
unaryPredicate name predicate args = do
    value <- unaryArg name args
    pure $ VBool (predicate value)

isBool :: Value -> Bool
isBool VBool{} = True
isBool _ = False

isString :: Value -> Bool
isString VStr{} = True
isString _ = False

isSymbol :: Value -> Bool
isSymbol VSym{} = True
isSymbol _ = False

isNull :: Value -> Bool
isNull VNil = True
isNull _ = False

isPair :: Value -> Bool
isPair VPair{} = True
isPair _ = False

isProcedure :: Value -> Bool
isProcedure VClosure{} = True
isProcedure VBuiltin{} = True
isProcedure VContinuation{} = True
isProcedure _ = False

builtinCallCC :: [Value] -> Eval Value
builtinCallCC _ =
    throwError $
        OtherEvalError "call/cc: internal continuation primitive used outside evaluator"

builtinListP :: [Value] -> Eval Value
builtinListP args = do
    value <- unaryArg "list?" args
    VBool <$> isProperList value

builtinNot :: [Value] -> Eval Value
builtinNot args = do
    value <- unaryArg "not" args
    pure $ case value of
        VBool False -> VBool True
        _ -> VBool False

builtinCons :: [Value] -> Eval Value
builtinCons args = do
    (carValue, cdrValue) <- binaryArgs "cons" args
    newPair carValue cdrValue

builtinCar :: [Value] -> Eval Value
builtinCar args = do
    value <- unaryArg "car" args
    (carRef, _) <- asPair "car" value
    liftIO $ readIORef carRef

builtinCdr :: [Value] -> Eval Value
builtinCdr args = do
    value <- unaryArg "cdr" args
    (_, cdrRef) <- asPair "cdr" value
    liftIO $ readIORef cdrRef

carCdrBuiltins :: [(Text, Value)]
carCdrBuiltins = mkComposedCarCdr <$> carCdrOperationNames

carCdrOperationNames :: [[Char]]
carCdrOperationNames = concatMap operationNamesOfLength [2 .. 4]
  where
    operationNamesOfLength :: Int -> [[Char]]
    operationNamesOfLength 0 = [""]
    operationNamesOfLength n = do
        op <- ['a', 'd']
        rest <- operationNamesOfLength (n - 1)
        pure (op : rest)

mkComposedCarCdr :: [Char] -> (Text, Value)
mkComposedCarCdr ops =
    mkBuiltin name (builtinComposedCarCdr name ops)
  where
    name = "c" <> T.pack ops <> "r"

builtinComposedCarCdr :: Text -> [Char] -> [Value] -> Eval Value
builtinComposedCarCdr name ops args = do
    value <- unaryArg name args
    foldM (applyCarCdrOp name) value (reverse ops)

applyCarCdrOp :: Text -> Value -> Char -> Eval Value
applyCarCdrOp name value op = do
    (carRef, cdrRef) <- asPair name value
    case op of
        'a' -> liftIO $ readIORef carRef
        'd' -> liftIO $ readIORef cdrRef
        _ -> throwError $ OtherEvalError ("internal car/cdr operation: " <> show op)

builtinList :: [Value] -> Eval Value
builtinList = listToValue

builtinDisplay :: [Value] -> Eval Value
builtinDisplay args = do
    displayValues args
    pure VUnspecified

builtinNewline :: [Value] -> Eval Value
builtinNewline [] = do
    liftIO $ TIO.putStr "\n"
    pure VUnspecified
builtinNewline _ = arityError "newline" "exactly 0 arguments"

displayValues :: [Value] -> Eval ()
displayValues =
    traverse_ $ \value -> do
        text <- liftIO $ displayValueIO value
        liftIO $ TIO.putStr text

displayValueIO :: Value -> IO Text
displayValueIO (VStr value) = pure value
displayValueIO value = showValueIO value

builtinLength :: [Value] -> Eval Value
builtinLength args = do
    value <- unaryArg "length" args
    VNum . fromInteger <$> properListLength value

builtinAppend :: [Value] -> Eval Value
builtinAppend [] = pure VNil
builtinAppend [value] = do
    _ <- asProperList "append" value
    pure value
builtinAppend (value : rest) = do
    values <- asProperList "append" value
    tailValue <- builtinAppend rest
    listWithTail values tailValue

builtinLast :: [Value] -> Eval Value
builtinLast args = do
    value <- unaryArg "last" args
    lastOfList value

builtinMemq :: [Value] -> Eval Value
builtinMemq args = do
    (needle, haystack) <- binaryArgs "memq" args
    go [] needle haystack
  where
    go :: [PairCell] -> Value -> Value -> Eval Value
    go _ _ VNil = pure $ VBool False
    go seen needle pair@(VPair carRef cdrRef) =
        let cell = (carRef, cdrRef)
         in if pairSeen cell seen
                then typeError "memq" "proper list" pair
                else do
                    carValue <- liftIO $ readIORef carRef
                    matched <- eqValue needle carValue
                    if matched
                        then pure pair
                        else do
                            cdrValue <- liftIO $ readIORef cdrRef
                            go (cell : seen) needle cdrValue
    go _ _ value = typeError "memq" "proper list" value

builtinSetCar :: [Value] -> Eval Value
builtinSetCar args = do
    (pairValue, newValue) <- binaryArgs "set-car!" args
    (carRef, _) <- asPair "set-car!" pairValue
    liftIO $ writeIORef carRef newValue
    pure VUnspecified

builtinSetCdr :: [Value] -> Eval Value
builtinSetCdr args = do
    (pairValue, newValue) <- binaryArgs "set-cdr!" args
    (_, cdrRef) <- asPair "set-cdr!" pairValue
    liftIO $ writeIORef cdrRef newValue
    pure VUnspecified

builtinStringAppend :: [Value] -> Eval Value
builtinStringAppend args = VStr . T.concat <$> traverse (asString "string-append") args

builtinSymbolToString :: [Value] -> Eval Value
builtinSymbolToString args = VStr <$> (unaryArg "symbol->string" args >>= asSymbol "symbol->string")

builtinStringToSymbol :: [Value] -> Eval Value
builtinStringToSymbol args = VSym <$> (unaryArg "string->symbol" args >>= asString "string->symbol")

builtinStringToNumber :: [Value] -> Eval Value
builtinStringToNumber args = do
    value <- unaryArg "string->number" args >>= asString "string->number"
    pure $ maybe (VBool False) VNum (parseNumberText value)

builtinNumberToString :: [Value] -> Eval Value
builtinNumberToString args = do
    value <- unaryArg "number->string" args >>= asNum "number->string"
    pure $ VStr (numberToText value)

builtinEqP :: [Value] -> Eval Value
builtinEqP args = do
    (a, b) <- binaryArgs "eq?" args
    VBool <$> eqValue a b

builtinNeqP :: [Value] -> Eval Value
builtinNeqP args = do
    (a, b) <- binaryArgs "neq?" args
    VBool . not <$> eqValue a b

builtinEqualP :: [Value] -> Eval Value
builtinEqualP args = do
    (a, b) <- binaryArgs "equal?" args
    VBool <$> equalValue a b

newPair :: Value -> Value -> Eval Value
newPair carValue cdrValue = do
    carRef <- liftIO $ newIORef carValue
    cdrRef <- liftIO $ newIORef cdrValue
    pure $ VPair carRef cdrRef

listToValue :: [Value] -> Eval Value
listToValue values = listWithTail values VNil

listWithTail :: [Value] -> Value -> Eval Value
listWithTail [] tailValue = pure tailValue
listWithTail (value : rest) tailValue = do
    restValue <- listWithTail rest tailValue
    newPair value restValue

asProperList :: Text -> Value -> Eval [Value]
asProperList name = go []
  where
    go :: [PairCell] -> Value -> Eval [Value]
    go _ VNil = pure []
    go seen value@(VPair carRef cdrRef) =
        let cell = (carRef, cdrRef)
         in if pairSeen cell seen
                then typeError name "proper list" value
                else do
                    carValue <- liftIO $ readIORef carRef
                    cdrValue <- liftIO $ readIORef cdrRef
                    (carValue :) <$> go (cell : seen) cdrValue
    go _ value = typeError name "proper list" value

isProperList :: Value -> Eval Bool
isProperList = go []
  where
    go :: [PairCell] -> Value -> Eval Bool
    go _ VNil = pure True
    go seen (VPair carRef cdrRef) =
        let cell = (carRef, cdrRef)
         in if pairSeen cell seen
                then pure False
                else do
                    cdrValue <- liftIO $ readIORef cdrRef
                    go (cell : seen) cdrValue
    go _ _ = pure False

properListLength :: Value -> Eval Integer
properListLength = go [] 0
  where
    go :: [PairCell] -> Integer -> Value -> Eval Integer
    go _ n VNil = pure n
    go seen n value@(VPair carRef cdrRef) =
        let cell = (carRef, cdrRef)
         in if pairSeen cell seen
                then typeError "length" "proper list" value
                else do
                    cdrValue <- liftIO $ readIORef cdrRef
                    go (cell : seen) (n + 1) cdrValue
    go _ _ value = typeError "length" "proper list" value

lastOfList :: Value -> Eval Value
lastOfList = go []
  where
    go :: [PairCell] -> Value -> Eval Value
    go _ VNil = throwError $ OtherEvalError "last: expected non-empty proper list"
    go seen value@(VPair carRef cdrRef) =
        let cell = (carRef, cdrRef)
         in if pairSeen cell seen
                then typeError "last" "proper list" value
                else do
                    carValue <- liftIO $ readIORef carRef
                    cdrValue <- liftIO $ readIORef cdrRef
                    case cdrValue of
                        VNil -> pure carValue
                        VPair{} -> go (cell : seen) cdrValue
                        other -> typeError "last" "proper list" other
    go _ value = typeError "last" "proper list" value

eqValue :: Value -> Value -> Eval Bool
eqValue a b =
    pure $ case (a, b) of
        (VNum x, VNum y) -> x == y
        (VBool x, VBool y) -> x == y
        (VStr x, VStr y) -> x == y
        (VSym x, VSym y) -> x == y
        (VNil, VNil) -> True
        (VUnspecified, VUnspecified) -> True
        (VPair carA cdrA, VPair carB cdrB) -> carA == carB && cdrA == cdrB
        (VBuiltin nameA _, VBuiltin nameB _) -> nameA == nameB
        _ -> False

equalValue :: Value -> Value -> Eval Bool
equalValue = equalValueWith []

equalValueWith :: [PairComparison] -> Value -> Value -> Eval Bool
equalValueWith seen (VPair carA cdrA) (VPair carB cdrB) =
    let left = (carA, cdrA)
        right = (carB, cdrB)
        comparison = (left, right)
     in if comparisonSeen comparison seen
            then pure True
            else do
                carValueA <- liftIO $ readIORef carA
                carValueB <- liftIO $ readIORef carB
                let nextSeen = comparison : seen
                carsEqual <- equalValueWith nextSeen carValueA carValueB
                if not carsEqual
                    then pure False
                    else do
                        cdrValueA <- liftIO $ readIORef cdrA
                        cdrValueB <- liftIO $ readIORef cdrB
                        equalValueWith nextSeen cdrValueA cdrValueB
equalValueWith _ a b = eqValue a b
