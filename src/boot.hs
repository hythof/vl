module Boot where

import Data.Char(isAlphaNum)
import Debug.Trace(trace)
import Control.Monad
import Control.Monad.Trans.State

--( Syntax tree )------------------------------------------
type Env = [(String, Exp)]
data Exp = Text String
  | Number Double
  | Lambda [String] [Exp] Exp -- args, binds, body
  | Ref String
  | Apply Exp [Exp]
  deriving Show

--( Parser )-----------------------------------------------

type Parser a = StateT String Maybe a

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  (x:xs) <- get
  guard $ f x
  put xs
  return x

char :: Char -> Parser Char
char c = satisfy (== c)

oneOf :: String -> Parser Char
oneOf xs = satisfy $ \x -> elem x xs

noneOf :: String -> Parser Char
noneOf xs = satisfy $ \x -> not $ elem x xs

string :: String -> Parser String
string text = check text
  where
    check [] = return text
    check (x:xs) = satisfy (== x) >> check xs

between :: Parser a -> Parser b -> Parser c -> Parser c
between left right center = do
  left
  matched <- center
  right
  return matched

many :: Parser a -> Parser [a]
many p = many_r p []
  where
    many_r :: Parser a -> [a] -> Parser [a]
    many_r f acc = (f >>= \x -> many_r f $ x : acc) `mplus` return (reverse acc)

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return $ x : xs

try :: Parser a -> Parser a
--try p = StateT $ \s -> case (runStateT p) s of
--  Just (a, ss) -> Just (a, ss)
--  Nothing -> put s >> Nothing
try p = do
  s <- get
  p `mplus` (put s >> fail "")

lexeme :: Parser a -> Parser a
lexeme p = do
  v <- p
  many $ oneOf " \r\n\t"
  return v

debug :: Parser a -> Parser a
debug p = do
  x <- get
  trace ("DEBUG(" ++ x ++ ")") p

--( parse and eval )---------------------------------------
run :: String -> String
run src = case lookup "main" env of
    Just main -> show $ eval env main
    Nothing -> show env
  where
    env = parse src

parse :: String -> Env
parse s = case runStateT parse_named_declears s of
  Just (env, _) -> env
  Nothing -> []

parse_named_declears = many $ do
  name <- read_token
  declear <- parse_declear
  return (name, declear)

parse_declear = parse_func

parse_func = do
  args <- many $ read_token
  lexeme $ char '='
  body <- parse_exp
  return $ case length args of
    0 -> body
    _ -> Lambda args [] body

parse_exp = try parse_op2
  `mplus` try parse_apply
  `mplus` parse_bottom
  `mplus` debug (error "exp")

parse_bottom = try parse_text
  `mplus` try parse_number
  `mplus` try parse_ref
  `mplus` (between (char '(') (char ')') parse_exp)
  `mplus` debug (error "bottom")

parse_apply = do
  ref <- parse_ref
  args <- many1 parse_exp
  return $ Apply ref args

parse_op2 :: Parser Exp
parse_op2 = do
  left <- parse_bottom
  op <- parse_op
  right <- parse_exp
  return $ Apply op [left, right]

parse_text :: Parser Exp
parse_text = Text <$> between (char '"') (char '"') (many $ noneOf "\"")

parse_number :: Parser Exp
parse_number = debug $ Number <$> ((many1 $ oneOf "0123456789.") >>= return . read)

parse_ref :: Parser Exp
parse_ref = Ref <$> read_token

read_token :: Parser String
read_token = lexeme $ many1 $ satisfy isAlphaNum

parse_op :: Parser Exp
parse_op = Ref <$> (lexeme $ many1 $ oneOf "+-*/<>?|~&%")

eval :: Env -> Exp -> Exp
eval _ e@(Text _) = e
eval _ e@(Number _) = e
eval _ e@(Lambda _ _ _) = e
eval scope (Ref name) = case lookup name scope of
    Just exp_ -> exp_
    Nothing -> error $ "Not found " ++ name ++ " in " ++ show scope
eval scope (Apply exp_ params) = case eval scope exp_ of
    Lambda args binds body -> eval ((zip args (binds ++ params)) ++ scope) body
    _ -> error $ "Panic " ++ show exp_ ++ " with " ++ show params

--( main )-------------------------------------------------
main = do
  putStr "\n-------------------------------------------\n\n"
  print $ runStateT parse_exp "12.0"
  --putStrLn $ run "main = \"hi\""
  --putStrLn $ run "main = 123"
  --putStrLn $ run "main = 1 + 1"
  --putStrLn $ run "main = s => s"
