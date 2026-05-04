{- | Initial environment construction.

This module wires the runtime frame primitives together with the builtin
procedure table. Keeping it separate lets 'Scheme.Evaluator' stay focused
on evaluating already-analyzed AST nodes.
-}
module Scheme.Environment (
    initialEnv,
) where

import Data.Map.Strict qualified as Map
import Scheme.AST (mkId)
import Scheme.Builtin (builtins)
import Scheme.Runtime (Env, bindInFrame, newFrame)

-- | Build a top-level environment populated with all builtins.
initialEnv :: IO Env
initialEnv = do
    frame <- newFrame
    for_ (Map.toList builtins) $ \(name, value) -> case mkId name of
        Just ident -> bindInFrame ident value frame
        -- Builtin names are static and known-valid identifiers; an empty
        -- name here indicates a programming error in 'Scheme.Builtin'.
        Nothing -> fail $ "initialEnv: invalid builtin name: " <> toString name
    pure frame
