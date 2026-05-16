{- | Initial environment construction.

This module wires the runtime frame primitives together with the builtin
procedure table. Keeping it separate lets 'Scheme.Evaluator' stay focused
on evaluating already-analyzed AST nodes.
-}
module Scheme.Environment (
    initialEnv,
) where

import Data.Set qualified as Set
import Scheme.AST (mkId)
import Scheme.Builtin (builtins)
import Scheme.Runtime (Env, bindInFrame, newFrame)

-- | Build a top-level environment populated with all builtins.
initialEnv :: IO Env
initialEnv = do
    frame <- newFrame
    case duplicateBuiltinName builtins of
        Just name -> fail $ "initialEnv: duplicate builtin name: " <> toString name
        Nothing -> pure ()
    for_ builtins $ \(name, value) -> case mkId name of
        Just ident -> bindInFrame ident value frame
        -- Builtin names are static and known-valid identifiers; an empty
        -- name here indicates a programming error in 'Scheme.Builtin'.
        Nothing -> fail $ "initialEnv: invalid builtin name: " <> toString name
    pure frame

duplicateBuiltinName :: [(Text, a)] -> Maybe Text
duplicateBuiltinName = go Set.empty
  where
    go :: Set.Set Text -> [(Text, a)] -> Maybe Text
    go _ [] = Nothing
    go seen ((name, _) : rest)
        | name `Set.member` seen = Just name
        | otherwise = go (Set.insert name seen) rest
