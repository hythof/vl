module Common where

import           Debug.Trace (trace)

--( Structure )------------------------------------------------------
data AST =
-- value
    Void
  | String String
  | Int Int
  | Real Double
  | Bool Bool
-- type
  | TypeStruct [String] -- field names
  | TypeEnum String [String] -- tag, field names
  | TypeState [String] [(String, AST)] -- field names, scope
-- container
  | List [AST]
  | Struct [(String, AST)]
  | Enum String AST
  | Return AST
-- expression
  | Func [String] AST
  | Ref String
  | Op2 String AST AST
  | Apply AST [AST]
  | Match [([AST], AST)]
  | Stmt [(String, AST)]
  | Update String String AST -- variable, op code, ast
-- runtime only
  | Error String
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
    Miss m      -> Miss m
    Hit val src -> Hit (f val) src

instance Applicative Parser where
  pure v = Parser $ \s -> Hit v s
  l <*> r = pmap2 l r $ \m n -> n {val = (val m) (val n)}

instance Monad Parser where
    return = pure
    m >>= f = pmap m $ \n -> case n of
                Hit val src -> runParser (f val) src
                Miss msg    -> Miss msg
    fail m = Parser $ \s -> Miss s

l <|> r = Parser $ \s -> case runParser l s of
  Hit val src -> Hit val src
  Miss _      -> runParser r s

az = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
num = "0123456789"
symbols = "_"
dot = "."

remaining_input = Parser $ \s -> Hit s s

satisfy f = Parser $ \s -> case check s of
  Just h  -> h
  Nothing -> Miss "failed"
 where
  check :: String -> Maybe (Result Char)
  check s = do
    guard $ (length s) > 0
    let c = s !! 0
    guard $ f c
    return $ Hit c (tail s)
  guard False = Nothing
  guard True  = Just ()

spaces = many $ oneOf " \t"
lexeme f = spaces >> f
oneOf xs = satisfy $ \x -> elem x xs
noneOf xs = satisfy $ \x -> not $ elem x xs
char x = satisfy (== x)
string []     = Parser $ \s -> Hit () s
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
read_op2 = lexeme $ many1 $ oneOf "+-*/.|&"
read_update_op2 = lexeme $ many1 $ oneOf "+-*/.|&:="
read_id = lexeme $ do
  prefix <- oneOf $ az ++ symbols
  remaining <- many $ oneOf (az ++ num ++ symbols ++ dot)
  return $ prefix : remaining
read_type = (read_between '[' ']' read_type)
            <|> (read_between '(' ')' read_type)
            <|> read_id
read_br = do
  many $ oneOf " \t"
  many1 $ oneOf "\r\n"
read_br1 = read_br >> string "  "
read_br2 = read_br >> string "    "
see f = Parser $ \s ->
  case runParser f s of
    Hit v _ -> Hit v s
    Miss m  -> Miss m

make_func [] ast   = ast
make_func args ast = Func args ast

parse :: String -> Env
parse input = case runParser parse_root (trim input) of
  Hit env ""   -> env
  Hit env left -> env ++ [("err", String $ "left: " ++ left)]
  Miss m       -> [("err", String m)]
 where
  trim s = reverse $ dropWhile (\x -> elem x " \t\r\n") (reverse s)

parse_root :: Parser Env
parse_root = sepBy parse_define read_br

parse_define :: Parser (String, AST)
parse_define = do
  name <- read_id
  case name of
    "struct" -> def_struct
    "enum"   ->  def_enum
    "state"  -> def_state
    _        -> def_func name
 where
  def_struct = do
    name <- read_id
    many read_type -- TODO: generics
    read_char ':'
    fields <- many1 (read_br1 >> def_line)
    return (name, TypeStruct fields)
  def_enum = do
    name <- read_id
    many read_type -- TODO: generics
    read_char ':'
    fields <- many1 (read_br1 >> enum_line name)
    return (name, Struct fields)
  def_state = do
    name <- read_id
    many read_type -- TODO: generics
    read_char ':'
    fields <- many1 (read_br1 >> def_line)
    exception_names <- many $ do { read_br1; name <- read_id; see read_br; return name }
    let exceptions = map (\x -> (x, Return $ Enum (name ++ "." ++ x) $ String "_error")) exception_names
    funcs <- many $ do { read_br1; name <- read_id; def_func name }
    return (name, TypeState fields $ exceptions ++ funcs)
  def_func name = do
    args <- many read_id
    read_char '='
    top <- parse_top
    return (name, make_func args top)
  def_line = do
    name <- read_id
    type_ <- read_type
    return name
  enum_line prefix = do
    name <- read_id
    fields <- enum_fields <|> (return [])
    let tag = prefix ++ "." ++ name
    return (name, TypeEnum tag fields)
  enum_fields = do
    read_char ':'
    many1 (read_br2 >> def_line)
  to_value "str"  = String ""
  to_value "int"  = Int 0
  to_value "real" = Real 0.0

parse_top = (read_br >> (parse_matches <|> parse_stmt))
  <|> parse_op2

parse_matches = Match <$> sepBy1 parse_match read_br
parse_match = do
  read_char '|'
  conds <- many parse_bot
  read_char '='
  body <- parse_op2
  return $ (conds, body)

parse_stmt = Stmt <$> sepBy1 parse_line read_br1
 where
  parse_line :: Parser (String, AST)
  parse_line = parse_assign <|> parse_call
  parse_assign = do
    name <- read_id
    op <- read_update_op2
    ast <- parse_op2
    let short_op = take ((length op) - 1) op
    case op of
      "=" -> return (name, ast)
      ":=" -> return (name, ast)
      _ -> return (name, Op2 short_op (Ref name) ast)
  parse_call = do
    ast <- parse_op2
    return ("", ast)

parse_op2 = do
  l <- parse_bot
  o <- read_op2 <|> return ""
  case o of
    "" -> return l
    _ -> do
      r <- parse_op2
      return $ Op2 o l r

parse_bot = parse_value
  <|> parse_call_or_ref

parse_value = parse_bool
  <|> parse_str
  <|> parse_num
  <|> parse_list

parse_str = (String <$> (fmap trim1 $ read_between '`' '`' (many $ noneOf "`")))
  <|> (String <$> read_between '"' '"' (many $ noneOf "\""))
 where
  trim1 s = reverse $ _trim1 $ reverse $ _trim1 s
  _trim1 ('\n':s) = s
  _trim1 s = s
parse_bool = Bool <$> do
  name <- read_id
  case name of
    "true"  -> return True
    "false" -> return False
    label   -> fail $ "miss " ++ label
parse_num = do
  spaces
  n <- many1 (oneOf $ '-' : num)
  (char '.' >> real n) <|> (int n)
 where
  int n = return $ Int (read n :: Int)
  real n = do
    m <- many1 (oneOf $ '-' : num)
    return $ Real (read (n ++ "." ++ m) :: Double)
parse_list = List <$> (read_between '[' ']' (many parse_bot))

parse_call_or_ref = do
  name <- read_id
  if name == "_"
  then return Void
  else (char '(' >> unit_call name) <|> (return $ Ref name)
 where
  unit_call name = do
    args <- many parse_bot
    read_char ')'
    return $ Apply (Ref name) args



--( Evaluator )-------------------------------------------------------
eval :: Env -> AST -> AST
eval env x@(Int _) = x
eval env x@(Real _) = x
eval env x@(Bool _) = x
eval env x@(String _) = x
eval env (Func [] ast) = ast
eval env x@(Func _ _) = x
eval env x@(Match _) = x
eval env x@(TypeStruct _) = x
eval env (TypeEnum tag []) = Enum tag $ Struct []
eval env x@(TypeEnum _ _) = x
eval env x@(TypeState _ _) = x
eval env x@(Struct _) = x
eval env x@(Enum _ _) = x
eval env x@(Return _) = x
eval env x@(Update _ _ _) = x
eval env (List xs) = List $ map (eval env) xs
eval env (Op2 op left right) = case (op, el) of
  (".", Struct fields) -> get_or_error right fields
  ("||", Bool False)   -> eval env right
  ("||", Bool True)    -> Bool True
  _                    -> f el er
 where
  get_or_error (Ref name) table = _get_or_error name table id
  get_or_error (Apply (Ref name) args) table = _get_or_error name table (\ast -> Apply ast args)
  _get_or_error name table f = case lookup name table of
    Just ast -> eval (table ++ env) (f ast)
    Nothing  -> Error $ "no field: " ++ name ++ " in " ++ (show table)
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
      glue "." = (++)
  f (List l) (List r) = List $ glue op l r
    where
      glue "++" = (++)
  f l r = Error $ "fail op: " ++ op ++ "\n- left: " ++ (show l) ++ "\n- right: " ++ (show r)
  el = eval env left
  er = eval env right
eval env (Apply target apply_args) = case eval env target of
  Func capture_args (Match conds) -> eval ((zip capture_args $ map (\(Enum _ v) -> v) evaled_args) ++ env) (match conds)
  Func capture_args body -> eval ((zip capture_args evaled_args) ++ env) body
  TypeStruct fields -> Struct $ zip fields evaled_args
  TypeEnum tag fields -> Enum tag $ Struct $ zip fields evaled_args
  TypeState fields state -> Struct $ (zip fields evaled_args) ++ state ++ env
  Match conds -> eval env (match conds)
  other -> other
 where
  evaled_args = map (eval env) apply_args
  match all_conds = _match all_conds
   where
    _match [] = Error $ "can't match " ++ (show all_conds) ++ " == " ++ (show evaled_args) ++ " from " ++ (show target)
    _match ((conds, body):rest) = if conds `equals` evaled_args
      then body
      else _match rest
  equals [] []         = True
  equals (x:xs) (y:ys) = equal x y && equals xs ys
  equals _ _           = False
  equal (Void) _               = True
  equal (Ref x) (TypeEnum y _) = x == y
  equal (Ref x) (Enum y _)     = x == y
  equal x y                    = x == y
eval env (Ref full_name) = find (split full_name '.') env
 where
  find :: [String] -> [(String, AST)] -> AST
  find [] env = Struct env
  find (name:rest) env = case lookup name env of
    Nothing -> Error $ "not found " ++ name ++ (if name == full_name then "" else " of " ++ full_name) ++ " in " ++ (show $ map fst env)
    Just (Struct fields) -> find rest fields
    Just ast -> case (length rest, eval env ast, length rest == 1 && is_digit (head rest)) of
      (0, ast, _) -> ast
      (1, String s, True) -> String [s !! (read (head rest) :: Int)]
      _ -> Error $ "invalid dot refrence " ++ full_name  ++ " of " ++ (show ast) ++ " rest: " ++ (show rest)
  is_digit []     = True
  is_digit (c:cs) = '0' <= c && c <= '9' && (is_digit cs)

eval env (Stmt lines) = exec_lines env lines
 where
  exec_lines env (line:lines) = case exec_line env line of
    (assign, Return ast) -> eval env ast
    (assign, Update name op ast) -> case op of
      ":=" -> exec_lines ((assign, ast) : env) lines
      _ -> exec_lines ((assign, eval_op2) : env) lines
     where
      eval_op2 = eval env $ Op2 op2 left right
      left = eval env $ Ref name
      right = ast
      op2 = take ((length op) - 1) op
    (assign, ast) -> exec_lines ((assign, ast) : env) lines
  exec_lines env [] = snd $ head $ env
  exec_line env (name, ast) = (name, eval env ast)
eval env x@(Error _) = x
eval env ast = error $ "yet: '" ++ (show ast) ++ "'"



--( Utility )------------------------------------------------------------
fmt (String s) = escape s
 where
  escape []        = []
  escape ('\r':cs) = "\\r" ++ (escape cs)
  escape ('\n':cs) = "\\n" ++ (escape cs)
  escape ('\t':cs) = "\\t" ++ (escape cs)
  escape (c:cs)    = c : (escape cs)
fmt (Int n) = show n
fmt (Real n) = show n
fmt (Bool True) = "true"
fmt (Bool False) = "false"
fmt (List l) = "[" ++ (join " " (map fmt l)) ++ "]"
fmt (Func args ast) = (join " " args) ++ " => " ++ (fmt ast)
fmt (Ref s) = s
fmt (Op2 o l r) = (fmt l) ++ " " ++ o ++ " " ++ (fmt r)
fmt (Apply body args) = (fmt body) ++ "(" ++ (join " " (map fmt args)) ++ ")"
fmt (Stmt ls) = "stmt: " ++ (show ls)
fmt (Update name op ast) = name ++ op ++ (fmt ast)
fmt (Return ast) = "return: " ++ (fmt ast)
fmt (Match m) = "match: " ++ (show m)
fmt (TypeStruct fields) = "type(" ++ (join ":" fields) ++ ")"
fmt (TypeEnum tag fields) = "enum." ++ tag ++ "(" ++ (join ":" fields) ++ ")"
fmt (TypeState fields state) = "state(" ++ (join " " fields) ++ ";  " ++ (fmt_env state) ++ ")"
fmt (Struct fields) = "(" ++ (fmt_env fields) ++ ")"
fmt (Enum tag (Struct [])) = tag
fmt (Enum tag val) = tag ++ (fmt val)
fmt (Void) = "_"
fmt (Error msg) = "ERROR(" ++ msg ++ ")"
fmt_env xs = (join "  " (map tie xs))
 where
  tie (k, v) = k ++ ":" ++ (fmt v)

dump src = do
  line "ast: " $ fmt ast
  line "ret: " $ fmt ret
  dump_env env
  line "src: " $ src
 where
  env = parse src
  ast = snd $ env !! 0
  ret = eval env ast
  line title body = do
    putStr title
    putStrLn body

dump_env env = do
  mapM_ (\(name, ast) -> putStrLn $ "env: " ++ name ++ "\t| " ++ (show ast)) env

debug x = do
  trace (show x) (return 0)
  s <- remaining_input
  trace ">>>" (return 0)
  trace s (return 0)
  trace "<<<" (return 0)

split :: String -> Char -> [String]
split s c = go s [] []
 where
  go [] [] acc = reverse acc
  go [] part acc = reverse ((reverse part : ) acc)
  go (x:xs) part acc = if x == c
    then go xs [] ((reverse part) : acc)
    else go xs (x : part) acc

join :: String -> [String] -> String
join glue xs = snd $ splitAt (length glue) splitted
 where
  splitted = foldl (\l r -> l ++ glue ++ r) "" xs
