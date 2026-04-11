module Sample.Types (SExpr (SNum, SSym, SNil, SPair)) where

-- | Minimal S-expression type for the sample interpreter.
data SExpr
    = SNum Integer
    | SSym Text
    | SNil
    | SPair SExpr SExpr
    deriving stock (Show, Eq)
