module Sample.Eval (eval) where

import Sample.Types (SExpr (SNil, SNum, SPair, SSym))

-- | Evaluate an S-expression. Only supports integer arithmetic.
eval :: SExpr -> Either Text SExpr
eval (SNum n) = Right (SNum n)
eval (SSym s) = Left $ "Unbound variable: " <> s
eval SNil = Right SNil
eval (SPair (SSym op) args) = case op of
    "+" -> applyArith sum 0 args
    "-" -> applySub args
    "*" -> applyArith product 1 args
    "/" -> applyDiv args
    _ -> Left $ "Unknown operator: " <> op
eval (SPair f _) = Left $ "Not a procedure: " <> show f

-- | Collect S-expression list into a Haskell list.
sexpToList :: SExpr -> Either Text [SExpr]
sexpToList SNil = Right []
sexpToList (SPair x rest) = (x :) <$> sexpToList rest
sexpToList other = Left $ "Not a proper list: " <> show other

-- | Expect a number.
expectNum :: SExpr -> Either Text Integer
expectNum (SNum n) = Right n
expectNum other = Left $ "Not a number: " <> show other

-- | Evaluate arguments and apply an arithmetic fold.
applyArith :: ([Integer] -> Integer) -> Integer -> SExpr -> Either Text SExpr
applyArith f _identity args = do
    exprs <- sexpToList args
    vals <- mapM (eval >=> expectNum) exprs
    Right $ SNum (f vals)

-- | Subtraction: requires at least one argument.
applySub :: SExpr -> Either Text SExpr
applySub args = do
    exprs <- sexpToList args
    vals <- mapM (eval >=> expectNum) exprs
    case vals of
        [] -> Left "'-' requires at least one argument"
        [x] -> Right $ SNum (negate x)
        (x : xs) -> Right $ SNum (x - sum xs)

-- | Division: exactly two arguments.
applyDiv :: SExpr -> Either Text SExpr
applyDiv args = do
    exprs <- sexpToList args
    vals <- mapM (eval >=> expectNum) exprs
    case vals of
        [_, 0] -> Left "Division by zero"
        [a, b] -> Right $ SNum (a `div` b {- HLINT ignore "Avoid partial function" -})
        _ -> Left "'/': expects exactly 2 arguments"
