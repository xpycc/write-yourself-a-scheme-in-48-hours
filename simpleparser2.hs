import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
  text <- getLine
  putStrLn $! readExpr text

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found value: " ++ [val]
