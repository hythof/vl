module Boot where

import Debug.Trace (trace)
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (State, runState, state, get, put)


--( Syntax tree )------------------------------------------

type Env = [(String, Exp)]
data Exp = Text String
  | Number Double
  | Lambda [String] [Exp] Exp -- args, binds, body
  | Ref String
  | Op2 String Exp Exp
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
  (Just v, s') -> (Just v, s')
  (Nothing, _) -> runState (runMaybeT r) s

spaces :: Parser String
spaces = many $ oneOf " \t"

lexeme :: Parser a -> Parser a
lexeme p = do
  v <- p
  many $ oneOf " \t"
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
parse s = case runState (runMaybeT parse_env) (s ++ "\n") of
  (Just env, _) -> env
  (Nothing, s) -> []

parse_env :: Parser Env
parse_env = many $ do
  name <- read_id
  declear <- parse_declear
  many1 ((many $ oneOf " \t") >> oneOf "\r\n")
  return (name, declear)

parse_declear :: Parser Exp
parse_declear = do
  args <- many $ read_id
  lexeme $ char '='
  body <- parse_top
  return $ case length args of
    0 -> body
    _ -> Lambda args [] body

parse_top :: Parser Exp
parse_top = do
  (exp_:args) <- many1 parse_exp
  return $ if length args == 0 then exp_ else Apply exp_ args

parse_exp :: Parser Exp
parse_exp = parse_lambda
  `orElse` parse_op2
  `orElse` parse_ref
  `orElse` parse_bottom

parse_bottom :: Parser Exp
parse_bottom = parse_text
  `orElse` parse_number
  `orElse` parse_ref
  `orElse` read_between "(" ")" parse_top

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
  return $ Op2 op left right

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

eval env (Ref name) = case lookup name env of
  Just exp_ -> eval env exp_
  Nothing -> error $ "Not found " ++ name ++ " in " ++ show_env env

eval env (Op2 op l r) = case (op, eval env l, eval env r) of
  ("+", Number a, Number b) -> Number $ a + b
  (op, ll, rr) -> error $ "Can't operate " ++ (show ll) ++ " " ++ op ++ " " ++ (show rr)

eval env (Apply exp_ params) = case eval env exp_ of
    Lambda args binds body -> eval ((zip args (binds ++ params)) ++ env) body
    _ -> error $ "Panic " ++ show exp_ ++ " with " ++ show params

show_env [] = ""
show_env ((name, exp_):xs) = "- " ++ name ++ " = " ++ (show exp_) ++ "\n" ++ show_env xs

--( main )-------------------------------------------------
detail :: String -> String -> IO ()
detail expect src = do
  putStrLn ""
  putStrLn $ "Expect | " ++ expect
  putStrLn $ "   Run | " ++ run src
  putStrLn $ " Input | " ++ src
  putStrLn $ show_env $ parse src
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
  test "55.0" "add a b = a + b\nmain = (add 1 2) + (add (add 3 4) 5) + (add 6 (add 7 8)) + 9 + 10"
  putStrLn "ok"
