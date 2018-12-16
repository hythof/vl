module Boot where

import           Control.Monad             (guard)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Control.Monad.Trans.State (State, get, put, runState, state)
import           Data.Fixed                (div', mod')
import           Data.List                 (intercalate, intersect, isPrefixOf,
                                            isSuffixOf, union, (\\))
import           Data.Maybe                (catMaybes)
import           Debug.Trace               (trace)

--( Syntax tree )------------------------------------------

type Env = [(String, Exp)]

data Exp =
-- value
    Char Char
  | String String
  | Int Int
  | Real Double
  | Bool Bool
-- container
  | Tuple [Exp]
  | List [Exp]
  | Map [(String, Exp)]
  | Struct [(String, Exp)]
-- expression
  | Func [Arg] Exp
  | Ref String
  | Op2 String Exp Exp
  | Apply Exp [Exp]
  | Stmt [Line]
-- enum
  | Enum String Exp
-- void
  | Void
-- runtime only
  | Error String
  | Agg [Exp]
  deriving (Show, Eq)

data Line =
    Call Exp
  | Def String Exp
  | Assign String Exp
  deriving (Show, Eq)

data Arg =
    ArgRef String
  | ArgType String String -- type name, capture name
  | ArgMatch Exp
  | ArgOpt String Exp
  deriving (Show, Eq)


--( parser library )-----------------------------------------------

type Parser a = MaybeT (State String) a

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  (x:xs) <- lift get
  guard $ f x
  lift $ put xs
  return x

look :: String -> Parser Bool
look s = do
  x <- lift get
  return $ isPrefixOf s x

char :: Char -> Parser Char
char c = satisfy (== c)

oneOf :: String -> Parser Char
oneOf xs = satisfy $ \x -> elem x xs

noneOf :: String -> Parser Char
noneOf xs = satisfy $ \x -> not $ elem x xs

string :: String -> Parser String
string text = check text
  where
    check []     = return text
    check (x:xs) = satisfy (== x) >> check xs

token :: Parser String
token = do
  f <- many1 $ oneOf "ABCDEFGHIJKLMNOPQRSTUVXWYZabcdefghijklmnopqrstuvxwyz_"
  b <- many $ oneOf "0123456789"
  return $ f ++ b

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = do
  x <- p
  xs <- many (sep >> p)
  return $ x : xs

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
  x <- p
  xs <- many1 (sep >> p)
  return $ x : xs

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

indent :: Parser String
indent = do
  spaces
  many $ oneOf "\r\n"
  spaces

indent1 :: Parser String
indent1 = do
  spaces
  many $ oneOf "\r\n"
  string "  "
  spaces

br :: Parser String
br = do
  many $ oneOf " \r\n"

spaces :: Parser String
spaces = many $ oneOf " \t"

white_spaces :: Parser String
white_spaces = many $ oneOf " \t\r\n"

lexeme :: Parser a -> Parser a
lexeme p = do
  v <- p
  many $ oneOf " \t"
  return v

read_char :: Char -> Parser Char
read_char = lexeme . char

read_string :: String -> Parser String
read_string = lexeme . string

read_token :: Parser String
read_token = lexeme $ token

read_args :: Parser [String]
read_args = many read_token

read_int :: Parser Int
read_int = lexeme $ ((many1 $ oneOf "0123456789") >>= return . read)

read_real :: Parser Double
read_real = do
  a <- many1 $ oneOf "0123456789"
  char '.'
  b <- lexeme $ many1 $ oneOf "0123456789"
  let str = a ++ "." ++ b
  let d = read str :: Double
  return d

read_op :: Parser String
read_op = lexeme $ (many1 $ oneOf "+-*/<>?|~&%")

read_between :: String -> String -> Parser a -> Parser a
read_between l r c = between (lexeme $ string l) (lexeme $ string r) c


--( parser )-----------------------------------------------

run :: String -> String
run src = case lookup "main" env of
    Just main -> format $ eval env main
    Nothing   -> show env
  where
    env = parse src
    number_format s = if isSuffixOf ".0" s then (take ((length s) - 2) s) else s
    bracket xs = "[" ++ (intercalate " " xs) ++ "]"
    --brace xs = "{" ++ (intercalate "; " xs) ++ "}"
    parentheses xs = "(" ++ (intercalate "; " xs) ++ ")"
    format x = case x of
      String v -> v
      Bool True -> "true"
      Bool False -> "false"
      Int v -> number_format $ show v
      Real v -> number_format $ show v
      Tuple xs -> intercalate ", " $ map format xs
      Struct xs -> parentheses $ map (\(k, v) -> k ++ ": " ++ (format v)) xs
      List xs -> bracket $ map format xs
      Map xs -> bracket $ map (\(k, v) -> k ++ ": " ++ (format v)) xs
      Ref a -> a
      Op2 op l r -> (format l) ++ " " ++ op ++ " " ++ (format r)
      Enum tag Void -> tag
      Enum tag value -> tag ++ " " ++ (format value)
      Func xs y -> (intercalate " " (map format_arg xs)) ++ " => " ++ format y
      _ -> show x
    format_arg (ArgRef name) = name
    format_arg (ArgType type_name ref_name) = "(" ++ type_name ++ " " ++ ref_name ++ ")"
    format_arg (ArgMatch exp) = show exp
    format_arg (ArgOpt name exp) = name ++ ":" ++ show exp

parse :: String -> Env
parse s = case runState (runMaybeT parse_env) (s ++ "\n") of
  (Just env, _) -> env
  (Nothing, s)  -> []

parse_env :: Parser Env
parse_env = parse_env_top []
  where
    parse_env_top :: Env -> Parser Env
    parse_env_top acc = MaybeT $ do
      result <- runMaybeT parse_nested_env
      case result of
       Nothing   -> return $ Just acc
       Just hits -> runMaybeT $ br >> parse_env_top (acc ++ hits)
    parse_nested_env :: Parser Env
    parse_nested_env = do
      name <- read_token
      case name of
        "enum" -> do
          name <- read_token
          many read_token -- drop type information
          read_char ':'
          lines <- many1 $ (indent1 >> parse_enum_line)
          return lines
        "type" -> do
          name <- read_token
          many read_token -- drop type information
          read_char ':'
          fields <- many1 $ (indent1 >> parse_type_line)
          return $ [(name, Struct fields)]
        _ -> do
          declear <- parse_declear
          many1 ((many $ oneOf " \t") >> oneOf "\r\n")
          return [(name, declear)]
      where
        parse_enum_line = do
          name <- read_token
          arg <- read_token `orElse` (return "")
          return (name, make_enum name arg)
        parse_type_line = do
          name <- read_token
          many read_token -- drop type information
          return (name, String "")
        make_enum name arg = if arg == ""
            then Enum name Void
            else Func [ArgRef arg] $ Enum name $ Ref arg

parse_declear :: Parser Exp
parse_declear = parse_declear_func
       `orElse` parse_declear_stmt

parse_declear_func :: Parser Exp
parse_declear_func = do
  args <- parse_args
  read_char '='
  exp <- parse_top
  return $ make_func args exp

parse_args :: Parser [Arg]
parse_args = many parse_arg

parse_arg :: Parser Arg
parse_arg = parse_opt
  `orElse` parse_ref
  `orElse` parse_type
  `orElse` parse_match
  where
    parse_ref = ArgRef <$> read_token
    parse_type = do
      read_char '('
      type_name <- read_token
      ref_name <- read_token `orElse` (return "")
      read_char ')'
      return $ ArgType type_name ref_name
    parse_match = ArgMatch <$> parse_exp
    parse_opt = do
      name <- token
      char ':'
      value <- parse_bottom
      return $ ArgOpt name value

parse_declear_stmt :: Parser Exp
parse_declear_stmt = do
  args <- parse_args
  read_char '='
  lines <- many1 (indent >> parse_line)
  return $ make_func args $ Stmt lines
  where
    parse_line = parse_assign
        `orElse` parse_def
        `orElse` parse_call
    parse_assign = do
      name <- read_token
      read_string "<="
      exp <- parse_exp
      return $ Assign name exp
    parse_def = do
      name <- read_token
      read_char '='
      exp <- parse_exp
      return $ Def name exp
    parse_call = do
      exp <- parse_exp
      return $ Call exp

parse_top :: Parser Exp
parse_top = do
  left <- parse_call
  MaybeT $ do
    m <- runMaybeT read_op
    case m of
      Nothing -> return $ Just left
      Just op -> do
        right <- runMaybeT parse_top
        case right of
          Nothing    -> error "fail parse top"
          Just right -> return $ Just $ Op2 op left right

parse_call :: Parser Exp
parse_call = do
  (exp_:args) <- many1 parse_exp
  return $ if length args == 0 then exp_ else Apply exp_ args

parse_exp :: Parser Exp
parse_exp = parse_func
  `orElse` parse_tuple
  `orElse` parse_op2
  `orElse` parse_apply
  `orElse` parse_bottom

parse_func :: Parser Exp
parse_func = do
  args <- sepBy read_token (read_char ',')
  lexeme $ string "=>"
  exp <- parse_exp
  return $ make_func (map ArgRef args) exp

parse_bottom :: Parser Exp
parse_bottom = parse_text
  `orElse` parse_real
  `orElse` parse_int
  `orElse` parse_bool
  `orElse` parse_ref
  `orElse` parse_struct
  `orElse` parse_map
  `orElse` parse_list
  `orElse` read_between "(" ")" parse_top

parse_tuple :: Parser Exp
parse_tuple = Tuple <$> sepBy1 parse_bottom (read_char ',')

parse_struct :: Parser Exp
parse_struct = Struct <$> read_between "{" "}" (sepBy1 func (read_char ';'))
  where
    func = do
      id <- read_token
      args <- parse_args
      read_char '='
      exp <- parse_exp
      return (id, make_func args exp)

parse_op2 :: Parser Exp
parse_op2 = do
  left <- parse_bottom
  op <- read_op
  right <- parse_exp
  return $ Op2 op left right

parse_map :: Parser Exp
parse_map = Map <$> (lexeme $ string "[:]" >> return [])
            `orElse` read_between "[" "]" (many1 kv)
  where
    kv = do
      id <- read_token
      read_char ':'
      exp <- parse_bottom
      return (id, exp)

parse_list :: Parser Exp
parse_list = List <$> read_between "[" "]" (many parse_exp)

parse_text :: Parser Exp
parse_text = String <$> lexeme (
             (between (char '"') (char '"') (many $ noneOf "\""))
    `orElse` (between (char '\'') (char '\'') (many $ noneOf "'")))

parse_int :: Parser Exp
parse_int = Int <$> read_int

parse_real :: Parser Exp
parse_real = Real <$> read_real

parse_apply :: Parser Exp
parse_apply = do
  id <- read_token
  args <- read_between "(" ")" (many1 (white_spaces >> parse_exp))
  return $ Apply (Ref id) args

parse_ref :: Parser Exp
parse_ref = Ref <$> read_token

parse_bool :: Parser Exp
parse_bool = Bool <$> lexeme (
                (string "true" >> return True)
       `orElse` (string "false" >> return False))

make_func [] exp   = exp
make_func args exp = Func args exp


--( evaluator )--------------------------------------------

eval :: Env -> Exp -> Exp
eval _ Void = Void
eval _ e@(String _) = e
eval _ e@(Int _) = e
eval _ e@(Real _) = e
eval _ e@(Bool _) = e
eval _ e@(Func _ _) = e
eval env e@(Agg xs) = Agg $ map (eval env) xs
eval env (Enum tag v) = Enum tag (eval env v)
eval env (Tuple xs) = Tuple $ map (eval env) xs
eval _ e@(Struct _) = e
eval env (List xs) = List $ map (eval env) xs
eval env (Map xs) = Map $ map (\(k, v) -> (k, eval env v)) xs
eval env (Stmt xs) = eval_line env xs (Error "not fond line in statement")
  where
    eval_line :: Env -> [Line] -> Exp -> Exp
    eval_line _ [] v = v
    eval_line env (line:lines) v = case line of
      Call exp ->  eval_line env lines $ eval env exp
      Def name exp -> eval_line ((name, eval env exp) : env) lines (Error "not found return in statement")
      Assign name exp -> eval_line ((name, eval env exp) : env) lines (Error "not found return in statement")

eval env (Ref name) = case map snd $ filter (\x -> fst x == name) env of
    [x] -> eval0 x
    xs -> Agg $ xs
  where
    eval0 e@(Func args body) = case apply0 args [] of
      Just hits -> eval (hits ++ env) body
      Nothing -> eval env e
    eval0 e = eval env e
    apply0 [] acc = Just acc
    apply0 ((ArgOpt name exp):xs) acc = apply0 xs $ (name, exp) : acc
    apply0 _ _ = Nothing

eval env (Op2 op l r) = case (eval env l, eval env r) of
    (Int a, Int b) -> Int $ int_op a b
    (Real a, Real b) -> Real $ real_op a b
    (Bool a, Bool b) -> Bool $ bool_op a b
    (Char a, Char b) -> String $ char_op a b
    (String a, String b) -> String $ string_op a b
    (List a, List b) -> List $ list_op a b
    (a, b) -> error $ "Can't operate " ++ (show a) ++ " " ++ op ++ " " ++ (show b)
  where
    int_op = case op of
      "+"  -> (+)
      "-"  -> (-)
      "*"  -> (*)
      "/"  -> (\a b -> fromIntegral (a `div'` b) :: Int)
      "//" -> (\a b -> fromIntegral (a `div'` b) :: Int)
      "%"  -> mod'
      "**" -> (^)
    real_op = case op of
      "+"  -> (+)
      "-"  -> (-)
      "*"  -> (*)
      "/"  -> (/)
      "//" -> (\a b -> fromIntegral (a `div'` b) :: Double)
      "%"  -> mod'
      "**" -> (**)
    char_op = case op of
      "+" -> (\a b -> a : [b])
    string_op = case op of
      "+" -> (++)
    bool_op = case op of
      "&&" -> (&&)
      "||" -> (||)
    list_op = case op of
      "+" -> (++)
      "-" -> (\\)
      "&" -> intersect
      "|" -> union

eval env (Apply (Ref "if") [cond, t, f]) = case eval env cond of
  Bool True  -> eval env t
  Bool False -> eval env f
  _          -> error $ "Condition is not boolean " ++ show cond
eval env (Apply exp_ params_) = top exp_
  where
    top (Func args body) = case apply args body of
      Just x -> x
      Nothing -> panic "func" [show exp_, show params, show env]
    top (Agg xs) = case apply_agg xs of
      Just x -> x
      Nothing -> panic "agg" [show exp_, show params, show env]
    top (Struct fields) = Struct $ zip (map fst fields) params_
    top (Ref name) = case map snd $ filter (\x -> fst x == name) env of
      [x] -> top x
      xs -> top $ Agg xs
    apply args body = case matches args params_ of
        Just (rest, hits) -> Just $ eval (hits ++ env) $ make_func rest body
        Nothing -> Nothing
    apply_agg [] = Nothing
    apply_agg ((Func args body):xs) = case apply args body of
      Just hit -> Just hit
      Nothing -> apply_agg xs
    matches :: [Arg] -> [Exp] -> Maybe ([Arg], [(String, Exp)])
    matches xxs yys = go xxs yys []
      where
        go :: [Arg] -> [Exp] -> [(String, Exp)] -> Maybe ([Arg], [(String, Exp)])
        go ((ArgOpt name exp):xs) [] acc = go xs [] $ (name, exp) : acc
        go remaining [] acc = Just (remaining, acc)
        go (x:xs) (y:ys) acc = case match x (eval env y) of
          Just hit -> go xs ys $ hit ++ acc
          Nothing -> Nothing
        go _ _ _ = Nothing
    match :: Arg -> Exp -> Maybe [(String, Exp)]
    match (ArgRef name) exp = Just [(name, exp)]
    match (ArgType type_name name) (Enum tag_name exp) =
      if type_name == tag_name
      then Just [(name, exp)]
      else Nothing
    match (ArgMatch exp1) exp2 = if exp1 == exp2 then Just [] else Nothing
    match (ArgOpt name _) exp = Just [(name, exp)]
    params = map (eval env) params_

eval env e = error $ "Does not support type " ++ show e

panic m xs = error $ "Panic: " ++ m ++ "\n  " ++ (intercalate "\n  " xs)

show_env [] = ""
show_env ((name, exp_):xs) = "* " ++ name ++ " = " ++ (show exp_) ++ "\n" ++ show_env xs


--( main )-------------------------------------------------
detail :: String -> String -> IO ()
detail expect src = do
  putStrLn $ "Expect | " ++ expect
  putStrLn $ "Actual | " ++ run src
  putStrLn $ "   Env |"
  putStr $ show_env $ parse src
  putStrLn $ "   Src |"
  putStrLn src
  putStrLn ""
  error "FAILED"

test :: String -> String -> IO ()
test expect src = if run src == expect then putStr "." else detail expect src

main :: IO ()
main = do
  -- values
  test "a" "main = 'a'"
  test "ab" "main = \"ab\""
  test "0" "main = 0"
  test "1" "main = 1"
  test "0.1" "main = 0.1"
  test "true" "main = true"
  test "false" "main = false"
  -- tuple
  test "1, 2, 3" "main = 1, 2, (1 + 2)"
  -- struct
  test "(a: 1; b: c => a + c)" "main = {a = 1; b c = a + c}"
  -- function
  test "x => x + 1" "main = x => x + 1"
  test "3" "main = (x => x + 1) 2"
  test "3" "main = (x, y => x + y) 1 2"
  test "y => x + y" "main = (x, y => x + y) 1"
  test "3" "main = (x, y => x + y) 1 2"
  test "0" "add3 a:1 b:2 c:3 = a + b + c\nmain = add3 0 0 0"
  test "3" "add3 a:1 b:2 c:3 = a + b + c\nmain = add3 0 0"
  test "5" "add3 a:1 b:2 c:3 = a + b + c\nmain = add3 0"
  test "6" "add3 a:1 b:2 c:3 = a + b + c\nmain = add3"
  -- containers
  test "[]" "main = []"
  test "[0]" "main = [0]"
  test "[1 2]" "main = [1 (1 + 1)]"
  test "[a: 1 b: 2]" "main = [a: 1 b: (1 + 1)]"
  -- branch
  test "2" "main = if false 1 2"
  test "3" "main = if false 1 (if false 2 3)"
  test "1" "f 1 = 1\nf 2 = 2\nmain = f 1"
  test "2" "f 1 = 1\nf 2 = 2\nmain = f 2"
  test "false" "zero 0 = false\nzero _ = true\nmain = zero 0"
  test "true" "zero 0 = false\nzero _ = true\nmain = zero 1"
  -- statement
  test "3" "main =\n  a = 1\n  b = 2\n  a + b"
  -- type
  test "(count: 1)" "type counter:\n  count int\nmain = counter 1"
  -- exp number
  test "3" "main = 1 + 2"
  test "-1" "main = 1 - 2"
  test "6" "main = 2 * 3"
  test "0.5" "main = 2.0 / 4.0"
  test "2" "main = 5 // 2"
  test "1" "main = 3 % 2"
  test "8" "main = 2 ** 3"
  test "5" "main = 3 + (4 / 2)"
  test "3.5" "main = (3.0 + 4.0) / 2.0"
  -- exp boolean
  test "true"  "main = true && true"
  test "false" "main = true && false"
  test "true"  "main = true || false"
  test "false" "main = false || false"
  -- exp list
  test "[]" "main = []"
  test "[0 1]" "main = [0] + [1]"
  test "[0]" "main = [0 1] - [1]"
  test "[0]" "main = [0 1] & [0]"
  test "[0 1]" "main = [0] | [0 1]"
  test "[0]" "main = [1 - 1]"
  -- enum
  test "none" "enum maybe a:\n  just a\n  none\nmain = none"
  test "just 1" "enum maybe a:\n  just a\n  none\nmain = just 1"
  test "just hi" "enum maybe a:\n  just a\n  none\nmain = just \"hi\""
  test "hi" "enum maybe a:\n  just a\n  none\nf (just a) = a\nf (none) = 0\nmain = f(just(\"hi\"))"
  test "0" "enum maybe a:\n  just a\n  none\nf (just a) = a\nf (none) = 0\nmain = f(none)"

  -- call
  test "55" "add a b = a + b\nmain = add(1 2) + add(add(3 4) 5) + add(6 add(7 8)) + 9 + 10"
  putStrLn "ok"
