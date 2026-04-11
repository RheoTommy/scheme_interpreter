module Sample.Printer (schemeShow) where

import Sample.Types (SExpr (SNil, SNum, SPair, SSym))

-- | Display a value in Scheme notation.
schemeShow :: SExpr -> Text
schemeShow (SNum n) = show n
schemeShow (SSym s) = s
schemeShow SNil = "()"
schemeShow (SPair a b) = "(" <> showPairInner a b <> ")"

showPairInner :: SExpr -> SExpr -> Text
showPairInner a SNil = schemeShow a
showPairInner a (SPair b c) = schemeShow a <> " " <> showPairInner b c
showPairInner a b = schemeShow a <> " . " <> schemeShow b
