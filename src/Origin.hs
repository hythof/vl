module Origin where

import           Debug.Trace               (trace)

--( Structure )------------------------------------------------------
data AST =
-- value
    Char Char
  | String String
  | Int Int
  | Real Double
  | Bool Bool
-- container
  | List [AST]
-- expression
  | Func [String] AST
  | Ref String
  | Op2 String AST AST
  | Apply AST [AST]
  | Stmt [Line]
-- enum
  | Enum String AST
-- state
  | Throw String
  | State [(String, AST)]
-- void
  | Void
-- runtime only
  | Closure [(String, AST)] AST
  deriving (Show, Eq, Ord)

data Line =
    Call AST
  | Assign String AST
  deriving (Show, Eq, Ord)

type Env = [(String, AST)]



--( Parser )---------------------------------------------------------
data Parser a = Parser {
                runParser :: String -> Maybe (a, String)
                }

instance Functor Parser where
    fmap f p = Parser $ \s ->
                fmap (\(s, v) -> (f s, v)) $ runParser p s

instance Applicative Parser where
    pure v = Parser $ \s -> Just (v, s)
    --l <*> r = Parser $ \s -> case runParser r s of
    --    Just (v, s') -> fmap (\(f, s'') -> (f v, s'')) $ runParser l s
    --    Nothing -> Nothing
    l <*> r = Parser $ \s -> do
      (v, s') <- runParser r s
      fmap (\(f, s'') -> (f v, s'')) $ runParser l s

instance Monad Parser where
    return = pure
    --m >>= f = Parser $ \s -> case runParser m s of
    --    Just (v, s') -> runParser (f v) s'
    --    Nothing -> Nothing
    m >>= f = Parser $ \s -> do
      (v, s') <- runParser m s
      runParser (f v) s'
    fail _ = Parser $ \_ -> Nothing

l <|> r = Parser $ \s -> case runParser l s of
  Just x -> return x
  Nothing -> runParser r s

guard False = Nothing
guard True = Just 0

remaining_input = Parser $ \s -> Just (s, s)

satisfy f = Parser $ \s -> do
  guard $ (length s) > 0
  let c = s !! 0
  guard $ f c
  Just (c, tail s)

lexeme f = do
  many $ ignore " \t\r\n"
  satisfy f

ignore [] = Parser $ \s -> Nothing
ignore (x:xs) = satisfy (== x) <|> ignore xs

oneOf [] = Parser $ \_ -> Nothing
oneOf (x:xs) = lexeme (== x) <|> oneOf xs

many1 f = do
  x <- f
  xs <- many f
  return $ x : xs

many f = many_r f []
many_r f acc = (many_acc f acc) <|> (return $ reverse acc)
many_acc f acc = do
  x <- f
  many_r f (x : acc)

sepBy1 f sep = do
  x <- f
  xs <- many (sep >> f)
  return $ x : xs
sepBy f sep = sepBy1 f sep <|> return []

char x = satisfy (== x)

read_white_spaces = many1 $ satisfy (\x -> elem x" \t\r\n")
read_brs = do
  many $ satisfy (\x -> elem x " \t")
  satisfy (\x -> elem x "\r\n")
  many $ satisfy (\x -> elem x " \t\r\n")
read_char x = lexeme (== x)
read_op = many1 $ oneOf "+-*/"
read_id = do
  let az = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  let num = "0123456789"
  let symbols = "_"
  prefix <- oneOf az
  remaining <- many $ oneOf (az ++ num ++ symbols)
  return $ prefix : remaining

make_func [] ast = ast
make_func args ast = Func args ast

parse :: String -> Env
parse input = case runParser parse_root (trim input) of
  Just (env, "") -> env
  Just (_, left) -> [("err", String $ "left: " ++ left)]
  Nothing -> [("err", String "failed")]
 where
  trim s = reverse $ dropWhile (\x -> elem x " \t\r\n") (reverse s)

parse_root :: Parser Env
parse_root = sepBy parse_define read_brs

parse_define :: Parser (String, AST)
parse_define = do
  name <- read_id
  ast <- def_func
  return $ (name, ast)
 where
  def_func = do
    args <- many read_id
    read_char '='
    ast <- parse_top
    return $ make_func args ast

parse_top = parse_op2
parse_bot = parse_call
  <|> parse_int

parse_call = do
  name <- read_id
  char '('
  args <- many parse_bot
  read_char ')'
  return $ Apply (Ref name) args

parse_op2 = do
  l <- parse_bot
  o <- read_op <|> return ""
  case o of
    "" -> return l
    _ -> do
      r <- parse_op2
      return $ Op2 o l r

parse_int = do
  s <- many1 (oneOf "0123456789")
  return $ Int (read s :: Int)



--( Evaluator )-------------------------------------------------------
eval :: Env -> AST -> AST
eval env x@(Int _) = x
eval env (Op2 op left right) = f el er
 where
  f (Int l) (Int r) = Int $ fi op l r
  fi "+" = (+)
  fi "-" = (-)
  el = eval env left
  er = eval env right
eval env ast = String $ ("'" ++ (show ast) ++ "'")



--( Main )------------------------------------------------------------
main = do
  src <- readFile "cc.vl"
  let env = parse src
  let ast = snd $ env !! 0
  let ret = eval env ast
  line "ast: " $ fmt ast
  line "ret: " $ fmt ret
  line "env: " $ join "\n  " (map (\(name, ast) -> name ++ " = " ++ (fmt ast)) env)
  line "src: " $ src
 where
  line title body = do
    putStr title
    putStrLn body

join :: String -> [String] -> String
join glue xs = snd $ splitAt (length glue) splitted
 where
  splitted = foldl (\l r -> l ++ glue ++ r) "" xs

fmt (Char c) = [c]
fmt (String s) = escape s
 where
  escape [] = []
  escape ('\r':cs) = "\\r" ++ (escape cs)
  escape ('\n':cs) = "\\n" ++ (escape cs)
  escape ('\t':cs) = "\\t" ++ (escape cs)
  escape (c:cs) = c : (escape cs)
fmt (Int n) = show n
fmt (Real n) = show n
fmt (Bool b) = show b
fmt (List l) = join ", " (map fmt l)
fmt (Func args ast) = (join " " args) ++ " => " ++ (fmt ast)
fmt (Ref s) = s
fmt (Op2 o l r) = (fmt l) ++ " " ++ o ++ " " ++ (fmt r)
fmt (Apply b a) = "apply"
fmt (Stmt ls) = "stmt"
fmt (Enum t e) = t ++ " " ++ (fmt e)
fmt (Throw s) = s
fmt (State xs) = fmt_env xs
fmt (Void) = "_"
fmt (Closure env b) = (fmt_env env) ++ (fmt b)
fmt_env xs = "[" ++ (join " " (map tie xs)) ++ "]"
 where
  tie (k, v) = k ++ ":" ++ (fmt v)

debug x = do
  trace (show x) (return 0)
  s <- remaining_input
  trace ">>>" (return 0)
  trace s (return 0)
  trace "<<<" (return 0)
