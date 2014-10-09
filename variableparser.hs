{-# LANGUAGE ExistentialQuantification #-}

import System.IO hiding (try)
import Control.Monad
import Control.Monad.Error
import Control.Applicative ((<$>))
import Data.IORef
import Text.ParserCombinators.Parsec hiding (spaces)

-- interact begin
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows . liftM show $
  liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = do
  initEnv <- nullEnv
  until_ (== "quit") (readPrompt "Lisp>>> ") (evalAndPrint initEnv)

main :: IO ()
main = runRepl
-- interact end

-- LispVal type begin
data LispVal  = Atom String
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Number Integer
              | String String
              | Bool Bool

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name)       = name
showVal (Number number)   = show number
showVal (Bool True)       = "#t"
showVal (Bool False)      = "#f"
showVal (List contents)   = "(" ++ (unwords $ map showVal contents) ++ ")"
showVal (DottedList h t)  = "(" ++ (unwords $ map showVal h) ++
                            " . " ++ showVal t ++ ")"
-- LispVal type end

-- LispError type begin
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++
  " args: found values " ++ (unwords $ map showVal found)
showError (TypeMismatch expected found) = "Invalid type: expected " ++
  expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parser error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError :: IOThrowsError String -> IOThrowsError String
trapError action = action `catchError` (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
-- LispError type end

-- parser begin
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t"      -> Bool True
    "#f"      -> Bool False
    otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [ Atom "quote", x ]

parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do
          char '('
          x <- try parseList <|> parseDottedList
          char ')'
          return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val
-- parser end

-- evaluation begin
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _)   = return val
eval env (Atom var)     = getVar env var
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool False  -> eval env alt
    otherwise   -> eval env conseq
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom func : args)) =
  mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $
  BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = case lookup func primitives of
  Nothing -> throwError $ NotFunction
    "Unrecognized primitive function args" func
  Just f  -> f args

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal]
                -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = Number . foldl1 op <$> mapM unpackNum params

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool)
              -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
  then throwError $ NumArgs 2 args
  else do
    left <- unpacker $ args !! 0
    right <- unpacker $ args !! 1
    return . Bool $ op left right

numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool
strBoolBinop = boolBinop unpackStr

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
  if null parsed then throwError . TypeMismatch "number" $ String n
    else return . fst . head $ parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s)  = return s
unpackStr (Number s)  = return $ show s
unpackStr (Bool b)    = return $ show b
unpackStr notString   = throwError $ TypeMismatch "string" notString

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badForm = throwError $ NumArgs 1 badForm

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (x:xs) _] = return $ List xs
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return . Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return . Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return . Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return . Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = mand (eqv [List xs,
  List ys]) (eqv [x, y])
    where
    mand e1 e2 = do
      Bool v1 <- e1
      Bool v2 <- e2
      return . Bool $ v1 && v2
eqv [List arg1, List arg2] = return . Bool $ length arg1 == length arg2
  && (and . map eqvPair $ zip arg1 arg2)
    where
    eqvPair (x1, x2) = case eqv [x1, x2] of
      Left err          -> False
      Right (Bool val)  -> val
eqv [_, _] = return . Bool $ False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpack) = 
  do
    val1 <- unpack arg1
    val2 <- unpack arg2
    return $ val1 == val2
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2)
    [AnyUnpacker unpackNum, AnyUnpacker unpackBool, AnyUnpacker unpackStr]
  if primitiveEquals
    then return . Bool $ primitiveEquals
    else eqv [arg1, arg2]
-- evaluation end

-- variable begin
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action =
  runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = do
  env <- readIORef envRef
  case lookup var env of
    Nothing   -> return False
    Just _    -> return True

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  case lookup var env of
    Nothing   -> throwError $ UnboundVar "Getting a unbound variable" var
    Just val  -> liftIO $ readIORef val

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  case lookup var env of
    Nothing -> throwError $ UnboundVar "Getting a unbound variable" var
    Just varRef -> liftIO $ writeIORef varRef value
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = do
  env <- readIORef envRef
  let addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)
  newEnv <- mapM addBinding bindings
  newIORef (newEnv ++ env)

-- variable end
