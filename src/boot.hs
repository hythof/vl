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
  | Lambda [String] Exp
  | Match [([Match], Exp)]
  | Ref String
  | Op2 String Exp Exp
  | Apply Exp [Exp]
  | Stmt [Line]
-- enum
  | Enum String Exp
-- void
  | Void
-- runtime error
  | Error String
  deriving (Show, Eq)

data Line = Call Exp
  | Def String Exp
  | Assign String Exp
  deriving (Show, Eq)

data Match = MatchExp Exp
--  | MatchType Type -- next version
  | MatchEnum String
--  | MatchArray [String] -- next version
--  | MatchTuple [String] -- next version
  | MatchAll
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

read_id :: Parser String
read_id = lexeme $ many1 $ oneOf "ABCDEFGHIJKLMNOPQRSTUVXWYZabcdefghijklmnopqrstuvxwyz_"

read_args :: Parser [String]
read_args = many read_id

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

debug :: String -> Parser Int
debug msg = do
  x <- lift get
  trace ("DEBUG: " ++ msg ++ " " ++ x) (return 1)

dump :: String -> Parser String
dump m = do
  x <- lift get
  trace ("DUMP: " ++ m ++ " " ++ x) (MaybeT $ state $ \s -> (Just "", s))



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
      Lambda args exp -> (intercalate ", " args) ++ " => " ++ format exp
      Tuple xs -> intercalate ", " $ map format xs
      Struct xs -> parentheses $ map (\(k, v) -> k ++ ": " ++ (format v)) xs
      List xs -> bracket $ map format xs
      Map xs -> bracket $ map (\(k, v) -> k ++ ": " ++ (format v)) xs
      Ref a -> a
      Op2 op l r -> (format l) ++ " " ++ op ++ " " ++ (format r)
      Enum tag Void -> tag
      Enum tag value -> tag ++ " " ++ (format value)
      _ -> show x

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
      name <- read_id
      case name of
        "enum" -> do
          name <- read_id
          many read_id -- drop type information
          read_char ':'
          lines <- many1 $ (indent1 >> parse_enum_line)
          return lines
        "type" -> do
          name <- read_id
          many read_id -- drop type information
          read_char ':'
          fields <- many1 $ (indent1 >> parse_type_line)
          return $ [(name, Struct fields)]
        _ -> do
          declear <- parse_declear
          many1 ((many $ oneOf " \t") >> oneOf "\r\n")
          return [(name, declear)]
      where
        parse_enum_line = do
          name <- read_id
          arg <- read_id `orElse` (return "")
          return (name, make_enum name arg)
        parse_type_line = do
          name <- read_id
          many read_id -- drop type information
          return (name, String "")
        make_enum name arg = if arg == ""
            then Enum name Void
            else Lambda [arg] $ Enum name $ Ref arg

parse_declear :: Parser Exp
parse_declear = parse_declear_func
       `orElse` parse_declear_if
       `orElse` parse_declear_stmt
       `orElse` parse_declear_value

parse_declear_func :: Parser Exp
parse_declear_func = do
  args <- read_args
  read_char '='
  exp <- parse_top
  return $ make_lambda args exp

parse_declear_if :: Parser Exp
parse_declear_if = do
  cond <- read_id
  lexeme $ read_char '='
  t <- do { char '\n'; lexeme $ char '|'; parse_call }
  f <- do { char '\n'; lexeme $ char '|'; parse_call }
  return $ make_lambda [cond] (Apply (Ref "if") [Ref cond, t, f])

parse_declear_stmt :: Parser Exp
parse_declear_stmt = do
  args <- read_args
  read_char '='
  lines <- many1 (indent >> parse_line)
  return $ make_lambda args $ Stmt lines
  where
    parse_line = parse_assign
        `orElse` parse_def
        `orElse` parse_call
    parse_assign = do
      name <- read_id
      read_string "<="
      exp <- parse_exp
      return $ Assign name exp
    parse_def = do
      name <- read_id
      read_char '='
      exp <- parse_exp
      return $ Def name exp
    parse_call = do
      exp <- parse_exp
      return $ Call exp

parse_declear_value :: Parser Exp
parse_declear_value = do
  cond <- read_id
  lexeme $ read_char '='
  matcher <- many $ do
    char '\n'
    lexeme $ char '|'
    args <- many parse_match
    read_char '='
    exp <- parse_call
    return (args, exp)
  return $ Match matcher
  where
     parse_match = do
       v <- parse_bottom
       return $ case v of
         Ref "_" -> MatchAll
         _       -> MatchExp v

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
parse_exp = parse_lambda
  `orElse` parse_tuple
  `orElse` parse_op2
  `orElse` parse_apply
  `orElse` parse_bottom

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

parse_lambda :: Parser Exp
parse_lambda = do
  args <- sepBy read_id (read_char ',')
  lexeme $ string "=>"
  exp <- parse_exp
  return $ make_lambda args exp

parse_tuple :: Parser Exp
parse_tuple = Tuple <$> sepBy1 parse_bottom (read_char ',')

parse_struct :: Parser Exp
parse_struct = Struct <$> read_between "{" "}" (sepBy1 func (read_char ';'))
  where
    func = do
      id <- read_id
      args <- read_args
      read_char '='
      exp <- parse_exp
      return (id, make_lambda args exp)

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
      id <- read_id
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
  id <- read_id
  args <- read_between "(" ")" (many1 (white_spaces >> parse_exp))
  return $ Apply (Ref id) args

parse_ref :: Parser Exp
parse_ref = Ref <$> read_id

parse_bool :: Parser Exp
parse_bool = Bool <$> lexeme (
                (string "true" >> return True)
       `orElse` (string "false" >> return False))

make_lambda [] exp   = exp
make_lambda args exp = Lambda args exp


--( evaluator )--------------------------------------------

eval :: Env -> Exp -> Exp
eval _ Void = Void
eval _ e@(String _) = e
eval _ e@(Int _) = e
eval _ e@(Real _) = e
eval _ e@(Bool _) = e
eval _ e@(Lambda _ _) = e
eval _ e@(Match _) = e
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

eval env (Ref name) = case lookup name env of
  Just exp_ -> eval env exp_
  Nothing   -> error $ "Not found " ++ name ++ " in \n" ++ show_env env

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
eval env (Apply exp_ params_) = case eval env exp_ of
    Lambda args body -> if lack then eval_ curry else eval_ body
      where
        pl = length params
        al = length args
        binds = zip args params
        lack = pl < al
        curry = Lambda (drop pl args) body
        eval_ = eval (binds ++ env)
    Match ms -> case exp_ of
      Ref name -> switch name ms
      _        -> switch "_" ms
    Struct fields -> Struct $ zip (map fst fields) params_
    _ -> error $ "Panic " ++ show exp_ ++ " with " ++ show params ++ " env " ++ (show env)
  where
    params = map (eval env) params_
    switch name [] = error $ "Nothing match " ++ name ++ " params=" ++ (show params)
    switch name all@((matchers, exp):rest) = if (length matchers) == (length params)
      then match env matchers params
      else error $ "Miss match length \n  " ++ (show params) ++ "\n  " ++ (show all)
      where
        match env [] [] = exp
        match env (x:xs) (y:ys) = case (x, y) of
          (MatchExp e, _) -> if e == y then match env xs ys else switch name rest
          -- TODO: modify variabl in env by matched enum name
          (MatchEnum enum, Enum tag e) -> if enum == tag then match env xs ys else switch name rest
          (MatchAll, _)   -> exp
          (_, _)          -> error $ "Bug in match process"

eval env e = error $ "Does not support type " ++ show e


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
  -- containers
  test "[]" "main = []"
  test "[0]" "main = [0]"
  test "[1 2]" "main = [1 (1 + 1)]"
  test "[a: 1 b: 2]" "main = [a: 1 b: (1 + 1)]"
  -- branch
  test "2" "main = if false 1 2"
  test "3" "main = if false 1 (if false 2 3)"
  test "1" "bool _ =\n| 1\n| 2\nmain = bool true"
  test "2" "bool _ =\n| 1\n| 2\nmain = bool false"
  test "false" "zero _ =\n| 0 = false\n| 1 = true\nmain = zero 0"
  test "true" "zero _ =\n| 0 = false\n| 1 = true\nmain = zero 1"
  test "true" "zero _ =\n| 0 = false\n| _ = true\nmain = zero 2"
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

  -- call
  test "55" "add a b = a + b\nmain = add(1 2) + add(add(3 4) 5) + add(6 add(7 8)) + 9 + 10"
  putStrLn "ok"
