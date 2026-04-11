module Sample.Interpreter (run) where

import Sample.Eval (eval)
import Sample.Parser (parse)
import Sample.Printer (schemeShow)

-- | Parse and evaluate a Text, returning a displayable result.
run :: Text -> Either Text Text
run input = do
    sexpr <- parse input
    result <- eval sexpr
    Right (schemeShow result)
