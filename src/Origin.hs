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
--  | Enum String AST
-- state
--  | Throw String
--  | State [(String, AST)]
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
  fmap f m = pmap m $ \m -> case m of
    Miss m -> Miss m
    Hit val src -> Hit (f val) src

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

az = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
num = "0123456789"
symbols = "_"

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
noneOf xs = satisfy $ \x -> not $ elem x xs
char x = satisfy (== x)
string [] = Parser $ \s -> Hit () s
string (x:xs) = char x >> string xs

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

read_between l r c = do
  read_char l
  hit <- lexeme c
  read_char r
  return hit
read_char x = lexeme $ satisfy (== x)
read_op = lexeme $ many1 $ oneOf "+-*/"
read_id = lexeme $ do
  prefix <- oneOf az
  remaining <- many $ oneOf (az ++ num ++ symbols)
  return $ prefix : remaining
read_br = do
  many $ oneOf " \t"
  many1 $ oneOf "\r\n"

make_func [] ast = ast
make_func args ast = Func args ast

parse :: String -> Env
parse input = case runParser parse_root (trim input) of
  Hit env "" -> env
  Hit env left -> env ++ [("err", String $ "left: " ++ left)]
  Miss m -> [("err", String m)]
 where
  trim s = reverse $ dropWhile (\x -> elem x " \t\r\n") (reverse s)

parse_root :: Parser Env
parse_root = sepBy parse_define read_br

parse_define :: Parser (String, AST)
parse_define = do
  name <- read_id
  args <- many read_id
  read_char '='
  top <- parse_top
  return $ (name, make_func args top)

parse_top = parse_op2

parse_op2 = do
  l <- parse_bot
  o <- read_op <|> return ""
  case o of
    "" -> return l
    _ -> do
      r <- parse_op2
      return $ Op2 o l r

parse_bot = parse_value
  <|> parse_call_or_ref

parse_value = parse_bool
  <|> parse_str
  <|> parse_char
  <|> parse_num
  <|> parse_list

parse_char = Char <$> read_between '\'' '\'' (satisfy (\_ -> True))
parse_str = String <$> read_between '"' '"' (many $ noneOf "\"")
parse_bool = Bool <$> do
  name <- read_id
  case name of
    "true" -> return True
    "false" -> return False
    label -> fail $ "miss " ++ label
parse_num = do
  spaces
  n <- many1 (oneOf num)
  (char '.' >> real n) <|> (int n)
 where
  int n = return $ Int (read n :: Int)
  real n = do
    m <- many1 (oneOf num)
    return $ Real (read (n ++ "." ++ m) :: Double)
parse_list = List <$> (read_between '[' ']' (many parse_bot))

parse_call_or_ref = do
  name <- read_id
  (char '(' >> unit_call name) <|> (return $ Ref name)
 where
  unit_call name = do
    args <- many parse_bot
    read_char ')'
    return $ Apply (Ref name) args



--( Evaluator )-------------------------------------------------------
eval :: Env -> AST -> AST
eval env x@(Int _) = x
eval env x@(Real _) = x
eval env x@(Char _) = x
eval env x@(Bool _) = x
eval env x@(String _) = x
eval env x@(Func _ _) = x
eval env (List xs) = List $ map (eval env) xs
eval env (Op2 op left right) = f el er
 where
  f (Int l) (Int r) = Int $ glue op l r
    where
      glue "+" = (+)
      glue "-" = (-)
      glue "*" = (*)
      glue "/" = \a b -> truncate $ (fromIntegral a) / (fromIntegral b)
  f (Real l) (Real r) = Real $ glue op l r
    where
      glue "+" = (+)
      glue "-" = (-)
      glue "*" = (*)
      glue "/" = (/)
  f (String l) (String r) = String $ glue op l r
    where
      glue "++" = (++)
  f (List l) (List r) = List $ glue op l r
    where
      glue "++" = (++)
  el = eval env left
  er = eval env right
eval env (Apply target apply_args) = case eval env target of
  Func capture_args body -> eval ((zip capture_args args) ++ env) body
  other -> other
 where
  args = map (eval env) apply_args
eval env (Ref name) = case lookup name env of
  Just ast -> eval env ast
  Nothing -> Error env $ "not found " ++ name
eval env ast = Error env $ "yet: '" ++ (show ast) ++ "'"



--( Main )------------------------------------------------------------
main = do
  run_test
  src <- readFile "cc.vl"
  dump src

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
fmt (Bool True) = "true"
fmt (Bool False) = "false"
fmt (List l) = "[" ++ (join " " (map fmt l)) ++ "]"
fmt (Func args ast) = (join " " args) ++ " => " ++ (fmt ast)
fmt (Ref s) = s
fmt (Op2 o l r) = (fmt l) ++ " " ++ o ++ " " ++ (fmt r)
fmt (Apply body args) = (fmt body) ++ "(" ++ (join " " (map fmt args)) ++ ")"
fmt (Stmt ls) = "stmt"
--fmt (Enum t e) = t ++ " " ++ (fmt e)
--fmt (Throw s) = s
--fmt (State xs) = fmt_env xs
fmt (Void) = "_"
fmt (Error env msg) = msg ++ " " ++ (fmt_env env)
fmt (Closure env b) = (fmt_env env) ++ (fmt b)
fmt_env xs = "[" ++ (join "    " (map tie xs)) ++ "]"
 where
  tie (k, v) = k ++ ":" ++ (fmt v)

dump src = do
  line "ast: " $ fmt ast
  line "ret: " $ fmt ret
  line "env: " $ join "\n  " (map (\(name, ast) -> name ++ " = " ++ (fmt ast) ++ "\t# " ++ show ast) env)
  line "src: " $ src
 where
  env = parse src
  ast = snd $ env !! 0
  ret = eval env ast
  line title body = do
    putStr title
    putStrLn body

debug x = do
  trace (show x) (return 0)
  s <- remaining_input
  trace ">>>" (return 0)
  trace s (return 0)
  trace "<<<" (return 0)

--( Test )------------------------------------------------------------
run_test = do
  test_values values_code [
      ("c", "c")
    , ("s", "s")
    , ("1", "i")
    , ("1.0", "r")
    , ("true", "bt")
    , ("false", "bf")
    , ("[]", "l0")
    , ("[c s 1 1.0 true false 2]", "l7")
    , ("3", "ref")
    ]
  putStrLn "ok"
 where
  values_code = unlines [
      "c = 'c'"
    , "s = \"s\""
    , "i = 1"
    , "r = 1.0"
    , "bt = true"
    , "bf = false"
    , "l0 = []"
    , "l7 = [c s i r bt bf add(i i)]"
    , "add x y = x + y"
    , "ref = add(1 2)"
    -- TODO: state
    ]
  test_value expect src = test expect $ "main = " ++ src
  test_values _ [] = return ()
  test_values common ((expect, src):rest) = do
    test_value expect $ src ++ "\n" ++ common
    test_values common rest
  test expect src = if expect == act
    then putStr "."
    else do
      putStrLn ""
      putStrLn $ "expect: " ++ expect
      putStrLn $ "actual: " ++ act
      putStrLn ""
      putStrLn src
      dump src
      fail $ "failed"
   where
    env = parse src
    ast = snd $ env !! 0
    ret = eval env ast
    act = fmt ret
