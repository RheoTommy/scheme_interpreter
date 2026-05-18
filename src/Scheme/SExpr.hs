module Scheme.SExpr (SExpr (SNum, SBool, SStr, SSym, SNil, SPair)) where

import Scheme.Number (Number)

{- | S-expression type. Output of the parser.
Also used for macro expansion and quote contents.
-}
data SExpr
    = SNum Number
    | SBool Bool
    | SStr Text
    | SSym Text
    | SNil
    | SPair SExpr SExpr
    deriving stock (Show, Eq)
