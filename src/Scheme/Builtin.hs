{- | Builtin procedures.

Exports the initial map of builtin bindings that populate the top-level
environment.

This step only implements integer arithmetic, numeric comparison, and
@number?@. Other builtins (list, string, eq, etc.) will be added in
later steps.
-}
module Scheme.Builtin (builtins) where

import Control.Monad.Except (throwError)
import Data.Map.Strict qualified as Map
import Scheme.Runtime (
    Eval,
    EvalError (ArityError, DivisionByZero, TypeError),
    Value (VBool, VBuiltin, VNum),
    showValueKind,
 )

{- | All builtins, keyed by Scheme name.

Uses 'Map.fromListWithKey' so that a duplicate name is caught when the
map is forced (at 'initialEnv'), rather than silently keeping one of the
bindings.
-}
builtins :: Map Text Value
builtins = Map.fromListWithKey dup bindings
  where
    bindings =
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
        ]
    dup name _ _ = error ("Scheme.Builtin.builtins: duplicate name: " <> name)

{- | Build a @(name, VBuiltin name fn)@ pair. Having a single source of
truth for the name prevents the key and the 'VBuiltin' label from drifting.
-}
mkBuiltin :: Text -> ([Value] -> Eval Value) -> (Text, Value)
mkBuiltin name fn = (name, VBuiltin name fn)

-- | Require a value to be a number; otherwise raise 'TypeError'.
asNum :: Text -> Value -> Eval Integer
asNum _ (VNum n) = pure n
asNum name v =
    throwError $ TypeError (name <> ": expected number, got " <> showValueKind v)

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
    case safeQuot n m of
        Just q -> pure (VNum q)
        Nothing -> throwError DivisionByZero
builtinDiv _ = throwError $ ArityError "/: expected exactly 2 arguments"

{- | Truncation-toward-zero integer division, matching R5RS @quotient@
semantics. Returns 'Nothing' on zero divisor.

HLint's partial group flags 'quot'; the guard here makes the usage total,
and the scoped ignore in .hlint.yaml acknowledges this.
-}
safeQuot :: Integer -> Integer -> Maybe Integer
safeQuot _ 0 = Nothing
safeQuot n m = Just (n `quot` m)

-- | Binary numeric comparison: @(op a b)@ returns a boolean.
numericCmp :: Text -> (Integer -> Integer -> Bool) -> [Value] -> Eval Value
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
