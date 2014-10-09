import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  text <- getLine
  putStrLn $! readExpr text

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found value: " ++ [val]
