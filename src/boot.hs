module Boot where

import Data.Char(isAlphaNum)
import Debug.Trace(trace)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
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

type Parser a = MaybeT (State String) a

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  (x:xs) <- lift get
  guard $ f x
  lift $ put xs
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
between left right center = match `orElse` fail ""
  where
    match = do
      left
      matched <- center
      right
      return matched

many :: Parser a -> Parser [a]
many p = (many1 p) `orElse` (MaybeT $ state $ \s -> (Just [], s))

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return $ x : xs

orElse :: Parser a -> Parser a -> Parser a
orElse l r = MaybeT $ state $ \s -> case runState (runMaybeT l) s of
  (Just _, _) -> (runState $ runMaybeT l) s
  (Nothing, _) -> (runState $ runMaybeT r) s

spaces :: Parser String
spaces = many $ oneOf " \r\n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
  v <- p
  many $ oneOf " \r\n\t"
  return v

read_id :: Parser String
read_id = lexeme $ many1 $ oneOf "ABCDEFGHIJKLMNOPQRSTUVXWYZabcdefghijklmnopqrstuvxwyz_"

read_num :: Parser Double
read_num = lexeme $ ((many1 $ oneOf "0123456789.") >>= return . read)

read_op :: Parser String
read_op = lexeme $ (many1 $ oneOf "+-*/<>?|~&%")

read_between :: String -> String -> Parser a -> Parser a
read_between l r c = between (lexeme $ string l) (lexeme $ string r) c

debug :: Parser a -> Parser a
debug p = do
  x <- lift get
  trace ("DEBUG: " ++ x) p

dump :: String -> Parser String
dump m = do
  x <- lift get
  trace ("DUMP: " ++ m ++ " " ++ x) (MaybeT $ state $ \s -> (Just "", s))

--( parse and eval )---------------------------------------
run :: String -> String
run src = case lookup "main" env of
    Nothing -> show env
    Just main -> case eval env main of
      Text v -> v
      Number v -> show v
      _ -> show $ eval env main
  where
    env = parse src

parse :: String -> Env
parse s = case runState (runMaybeT parse_named_declears) s of
  (Just env, _) -> env
  (Nothing, s) -> []

parse_named_declears = many $ do
  name <- read_id
  declear <- parse_declear
  return (name, declear)

parse_declear = parse_func

parse_func = do
  args <- many $ read_id
  lexeme $ char '='
  body <- parse_top
  return $ case length args of
    0 -> body
    _ -> Lambda args [] body

parse_top = do
  (exp_:args) <- many1 parse_exp
  return $ if length args == 0 then exp_ else Apply exp_ args

parse_exp = read_between "(" ")" parse_exp
  `orElse` parse_lambda
  `orElse` parse_op2
  `orElse` parse_ref
  `orElse` parse_bottom

parse_bottom = parse_text
  `orElse` parse_number
  `orElse` parse_ref

parse_lambda :: Parser Exp
parse_lambda = do
  arg <- read_id
  lexeme $ string "=>"
  body <- parse_exp
  return $ Lambda [arg] [] body

parse_op2 :: Parser Exp
parse_op2 = do
  left <- parse_bottom
  op <- read_op
  right <- parse_exp
  return $ Apply (Ref op) [left, right]

parse_text :: Parser Exp
parse_text = Text <$> lexeme (between (char '"') (char '"') (many $ noneOf "\""))

parse_number :: Parser Exp
parse_number = Number <$> read_num

parse_ref :: Parser Exp
parse_ref = Ref <$> read_id

eval :: Env -> Exp -> Exp
eval _ e@(Text _) = e
eval _ e@(Number _) = e
eval _ e@(Lambda _ _ _) = e
eval scope (Ref name) = case lookup name scope of
    Just exp_ -> eval scope exp_
    Nothing -> error $ "Not found " ++ name ++ " in " ++ show_scope scope
  where
    show_scope [] = ""
    show_scope ((name, exp_):xs) = name ++ " " ++ (show exp_) ++ "\n" ++ show_scope xs
eval scope (Apply (Ref "+") [Number a, Number b]) = Number $ a + b
eval scope (Apply (Ref "+") [l, r]) = case (eval scope l, eval scope r) of
  (Number a, Number b) -> Number $ a + b
  (ll, rr) -> error $ "Can't add " ++ (show ll) ++ (show rr)
eval scope (Apply exp_ params) = case eval scope exp_ of
    Lambda args binds body -> eval ((zip args (binds ++ params)) ++ scope) body
    _ -> error $ "Panic " ++ show exp_ ++ " with " ++ show params

--( main )-------------------------------------------------
detail :: String -> String -> IO ()
detail expect src = do
  putStrLn ""
  putStrLn $ "Expect | " ++ expect
  putStrLn $ "   Run | " ++ run src
  putStrLn $ " Input | " ++ src
  putStrLn $ "   Env | " ++ (show $ parse src)
  error "fail"

test :: String -> String -> IO ()
test expect src = if run src == expect then putStr "." else detail expect src

main :: IO ()
main = do
  test "hi" "main = \"hi\""
  test "123.0" "main = 123"
  test "0.1" "main = 0.1"
  test "2.0" "main = 1 + 1"
  test "2.0" "main = (s => s + 1) 1"
  putStrLn "done"
