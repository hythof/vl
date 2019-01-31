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
  | Error Env String
  | Closure [(String, AST)] AST
  deriving (Show, Eq, Ord)

data Line =
    Call AST
  | Assign String AST
  deriving (Show, Eq, Ord)

type Env = [(String, AST)]



--( Parser )---------------------------------------------------------
data Result a = Hit { val :: a, src :: String }
              | Miss { src :: String }
data Parser a = Parser { runParser :: String -> Result a }

pmap m f = Parser $ \s -> f $ runParser m s
pmap2 m n f = Parser $ \s -> f (runParser m s) (runParser n s)

instance Functor Parser where
  fmap f m = pmap m $ \m -> m {val = f $ val m}

instance Applicative Parser where
  pure v = Parser $ \s -> Hit v s
  l <*> r = pmap2 l r $ \m n -> n {val = (val m) (val n)}

instance Monad Parser where
    return = pure
    m >>= f = pmap m $ \n -> case n of
                Hit val src -> runParser (f val) src
                Miss msg -> Miss msg
    fail m = Parser $ \s -> Miss s

l <|> r = Parser $ \s -> case runParser l s of
  Hit val src -> Hit val src
  Miss _ -> runParser r s

remaining_input = Parser $ \s -> Hit s s

satisfy f = Parser $ \s -> case check s of
  Just h -> h
  Nothing -> Miss "failed"
 where
  check :: String -> Maybe (Result Char)
  check s = do
    guard $ (length s) > 0
    let c = s !! 0
    guard $ f c
    return $ Hit c (tail s)
  guard False = Nothing
  guard True = Just ()

spaces = many $ oneOf " \t"
lexeme f = spaces >> f
oneOf xs = satisfy $ \x -> elem x xs

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

read_brs = do
  many $ oneOf " \t"
  satisfy (\x -> elem x "\r\n")
read_char x = lexeme $ satisfy (== x)
read_op = lexeme $ many1 $ oneOf "+-*/"
read_id = lexeme $ do
  let az = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  let num = "0123456789"
  let symbols = "_"
  prefix <- oneOf az
  remaining <- many $ oneOf (az ++ num ++ symbols)
  return $ prefix : remaining

make_func [] ast = ast
make_func args ast = Func args ast
branch [] other = return other
branch ((cond, body):rest) other = (cond >> body) <|> branch rest other

parse :: String -> Env
parse input = case runParser parse_root (trim input) of
  Hit env "" -> env
  Hit env left -> env ++ [("err", String $ "left: " ++ left)]
  Miss m -> [("err", String m)]
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
parse_bot = parse_int
  <|> parse_unit

parse_unit = do
  name <- read_id
  branch [
      (char '(', unit_call name)
    ] (Ref name)
 where
  unit_call name = do
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
  spaces
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
eval env (Apply target apply_args) = case eval env target of
  Func capture_args body -> eval ((zip capture_args args) ++ env) body
  other -> other
 where
  args = map (eval env) apply_args
eval env (Ref name) = case lookup name env of
  Just ast -> ast
  Nothing -> Error env $ "not found " ++ name
eval env ast = Error env $ "yet: '" ++ (show ast) ++ "'"



--( Main )------------------------------------------------------------
main = do
  src <- readFile "cc.vl"
  let env = parse src
  let ast = snd $ env !! 0
  let ret = eval env ast
  line "ast: " $ fmt ast
  line "ret: " $ fmt ret
  line "env: " $ join "\n  " (map (\(name, ast) -> name ++ " = " ++ (fmt ast) ++ "\t# " ++ show ast) env)
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
fmt (Apply body args) = (fmt body) ++ "(" ++ (join " " (map fmt args)) ++ ")"
fmt (Stmt ls) = "stmt"
fmt (Enum t e) = t ++ " " ++ (fmt e)
fmt (Throw s) = s
fmt (State xs) = fmt_env xs
fmt (Void) = "_"
fmt (Error env msg) = msg ++ " " ++ (fmt_env env)
fmt (Closure env b) = (fmt_env env) ++ (fmt b)
fmt_env xs = "[" ++ (join "    " (map tie xs)) ++ "]"
 where
  tie (k, v) = k ++ ":" ++ (fmt v)

debug x = do
  trace (show x) (return 0)
  s <- remaining_input
  trace ">>>" (return 0)
  trace s (return 0)
  trace "<<<" (return 0)
