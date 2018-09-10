module Boot where

import Debug.Trace (trace)
import Data.Fixed (div', mod')
import Data.List (isSuffixOf, intercalate, intersect, union, (\\))
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (State, runState, state, get, put)

--( Syntax tree )------------------------------------------

type Env = [(String, Exp)]

data Exp =
-- value
    Char Char
  | String String
  | Int Int -- TODO: Implement
  | Real Double
  | Bool Bool
-- container
  | Tuple [Exp]
  | List [Exp]
  | Map [(String, Exp)]
  | Struct [(String, Exp)]
-- expression
  | Lambda [String] Exp
  | Match [([Match], Exp)] -- TODO: Implement
  | Ref String
  | Op2 String Exp Exp
  | Apply Exp [Exp]
  | Statement [Line] -- TODO: Implement
-- meta information
  | Package [(String, Type)] [(String, Exp)] -- TODO: Implement
  deriving (Show, Eq)

-- TODO: following implements
data Type =
-- primitive
    TypeInt
  | TypeFloat
  | TypeString
  | TypeFunc [Type] Type
-- container
  | TypeTuple [Type]
  | TypeArray Type
  | TypeHash Type Type
  | TypeStruct [(String, Type)]
  | TypeEnum [(String, Type)]
-- lambda
  | TypeDeclare [Type] Type
-- type inference
  | TypeUndef [String] -- inference
  deriving (Show, Eq)

data Line = Call Exp
  | Def String Exp
  | Assign [String] Exp
  | Mutable String Type
  deriving (Show, Eq)

data Match = MatchExp Exp
  | MatchType Type String
  | MatchArray [String]
  | MatchTuple [String]
  | MatchAll
  deriving (Show, Eq)


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

read_id :: Parser String
read_id = lexeme $ many1 $ oneOf "ABCDEFGHIJKLMNOPQRSTUVXWYZabcdefghijklmnopqrstuvxwyz_"

read_args :: Parser [String]
read_args = many read_id

read_num :: Parser Double
read_num = lexeme $ ((many1 $ oneOf "0123456789.") >>= return . read)

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
    Nothing -> show env
  where
    env = parse src
    number_format s = if isSuffixOf ".0" s then (take ((length s) - 2) s) else s
    bracket xs = "[" ++ (intercalate " " xs) ++ "]"
    brace xs = "{" ++ (intercalate "; " xs) ++ "}"
    format x = case x of
      String v -> v
      Bool True -> "true"
      Bool False -> "false"
      Int v -> number_format $ show v
      Real v -> number_format $ show v
      Lambda args exp -> (intercalate ", " args) ++ " => " ++ format exp
      Tuple xs -> intercalate ", " $ map format xs
      Struct xs -> brace $ map (\(k, v) -> k ++ " = " ++ (format v)) xs
      List xs -> bracket $ map format xs
      Map xs -> bracket $ map (\(k, v) -> k ++ ": " ++ (format v)) xs
      Ref a -> a
      Op2 op l r -> (format l) ++ " " ++ op ++ " " ++ (format r)
      _ -> show x

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
  args <- read_args
  read_char '='
  exp <- parse_top
  return $ make_lambda args exp

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
          Nothing -> error "fail parse top"
          Just right -> return $ Just $ Op2 op left right

parse_call :: Parser Exp
parse_call = do
  (exp_:args) <- many1 parse_exp
  return $ if length args == 0 then exp_ else Apply exp_ args

parse_exp :: Parser Exp
parse_exp = parse_lambda
  `orElse` parse_match
  `orElse` parse_tuple
  `orElse` parse_op2
  `orElse` parse_apply
  `orElse` parse_bottom

parse_bottom :: Parser Exp
parse_bottom = parse_text
  `orElse` parse_number
  `orElse` parse_bool
  `orElse` parse_ref
  `orElse` parse_struct
  `orElse` parse_map
  `orElse` parse_list
  `orElse` read_between "(" ")" parse_top

parse_match :: Parser Exp
parse_match = do
  matches <- flip sepBy1 white_spaces $ do
    --conds <- sepBy1 parse_bottom (read_char ',')
    conds <- sepBy parse_cond (read_char ',')
    lexeme $ string "=>"
    exp <- parse_exp
    return (conds, exp)
  return $ Match matches
  where
    parse_cond :: Parser Match
    parse_cond = MatchExp <$> parse_bottom
      --TODO <|> MatchType Type String
      --TODO <|> MatchArray [String]
      --TODO <|> MatchTuple [String]
      --TODO <|> MatchAll

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

parse_number :: Parser Exp
parse_number = Real <$> read_num

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

make_lambda [] exp = exp
make_lambda args exp = Lambda args exp


--( evaluator )--------------------------------------------

eval :: Env -> Exp -> Exp
eval _ e@(String _) = e
eval _ e@(Int _) = e
eval _ e@(Real _) = e
eval _ e@(Bool _) = e
eval _ e@(Lambda _ _) = e
eval env (Tuple xs) = Tuple $ map (eval env) xs
eval _ e@(Struct _) = e
eval env (List xs) = List $ map (eval env) xs
eval env (Map xs) = Map $ map (\(k, v) -> (k, eval env v)) xs

eval env (Ref name) = case lookup name env of
  Just exp_ -> eval env exp_
  Nothing -> error $ "Not found " ++ name ++ " in \n" ++ show_env env

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
      "+" -> (+)
      "-" -> (-)
      "*" -> (*)
      "//" -> (\a b -> fromIntegral (a `div'` b) :: Int)
      "%" -> mod'
    real_op = case op of
      "+" -> (+)
      "-" -> (-)
      "*" -> (*)
      "/" -> (/)
      "//" -> (\a b -> fromIntegral (a `div'` b) :: Double)
      "%" -> mod'
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
  Bool True -> eval env t
  Bool False -> eval env f
  _ -> error $ "Condition is not boolean " ++ show cond
eval env (Apply exp_ params) = case eval env exp_ of
    Lambda args body -> if lack then eval_ curry else eval_ body
      where
        pl = length params
        al = length args
        binds = zip args params
        lack = pl < al
        curry = Lambda (drop pl args) body
        eval_ = eval (binds ++ env)
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
  -- values
  test "a" "main = 'a'"
  test "ab" "main = \"ab\""
  test "0" "main = 0"
  test "0.1" "main = 0.1"
  test "true" "main = true"
  test "false" "main = false"
  -- tuple
  test "1, 2, 3" "main = 1, 2, (1 + 2)"
  -- struct
  test "{a = 1; b = c => a + c}" "main = {a = 1; b c = a + c}"
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
  -- exp number
  test "3" "main = 1 + 2"
  test "-1" "main = 1 - 2"
  test "6" "main = 2 * 3"
  test "0.5" "main = 2 / 4"
  test "2" "main = 5 // 2"
  test "1" "main = 3 % 2"
  test "8" "main = 2 ** 3"
  test "5" "main = 3 + (4 / 2)"
  test "3.5" "main = (3 + 4) / 2"
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

  -- call
  test "55" "add a b = a + b\nmain = add(1 2) + add(add(3 4) 5) + add(6 add(7 8)) + 9 + 10"
  putStrLn "ok"
