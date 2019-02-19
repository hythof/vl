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
-- container
  | List [AST]
  | Struct [(String, AST)]
  | Enum String AST
-- expression
  | Func [String] AST
  | Ref String
  | Op2 String AST AST
  | Apply AST [AST]
  | Dot AST String [AST]
  | Match [([AST], AST)]
  | Stmt [(String, AST)] [(String, AST)] -- variables, statements
  | Return AST
  | Update String String AST -- variable, op code, ast
  | Closure [AST] AST
  deriving (Show, Eq, Ord)

type Env = [(String, AST)]



--( Parser )---------------------------------------------------------
data Result a = Hit { val :: a, src :: String }
              | Miss { src :: String }
data Parser a = Parser { runParser :: String -> Result a }

pmap m f = Parser $ \s -> case runParser m s of
  Hit val src -> f val src
  Miss msg    -> Miss msg
pmap2 m n f = Parser $ \s -> f (runParser m s) (runParser n s)

instance Functor Parser where
  fmap f m = pmap m $ \val src -> Hit (f val) src

instance Applicative Parser where
  pure v = Parser $ \s -> Hit v s
  l <*> r = pmap2 l r $ \m n -> n {val = (val m) (val n)}

instance Monad Parser where
  return = pure
  m >>= f = pmap m $ \val src -> runParser (f val) src
  fail m = Parser $ \s -> Miss s

l <|> r = Parser $ \s -> case runParser l s of
  Hit val src -> Hit val src
  Miss _      -> runParser r s

-- core
az = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
num = "0123456789"
symbols = "_"
dot = "."

remaining_input = Parser $ \s -> Hit s s

satisfy f = Parser $ \s -> go s
  where
    go "" = Miss "eof"
    go s  = if f (s !! 0)
      then Hit (s !! 0) (tail s)
      else Miss "miss"

some xs = satisfy $ \x -> elem x xs
none xs = satisfy $ \x -> not $ elem x xs
noop = Parser $ \s -> Hit () s

-- primitive
char x = satisfy (== x)
string []     = Parser $ \s -> Hit () s
string (x:xs) = char x >> string xs
look_ahead f = Parser $ \s ->
  case runParser f s of
    Hit v _ -> Hit v s
    Miss m  -> Miss m

-- combination
lexeme f = spaces >> f
many1 f = do
  x <- f
  xs <- many f
  return $ x : xs
many f = many_r f []
many_r f acc = option (reverse acc) (many_acc f acc)
many_acc f acc = do
  x <- f
  many_r f (x : acc)
sepBy1 f sep = do
  x <- f
  xs <- many (sep >> f)
  return $ x : xs
sepBy f sep = option [] $ sepBy1 f sep
between l r c = do
  l
  hit <- c
  r
  return hit
option fallback f = f <|> (return fallback)

-- helper
spaces = many $ some " \t"
spaces1 = many1 $ some " \t"
parenthesis l r c = do
  l
  matches <- many $ (skip_white_spaces >> c)
  skip_white_spaces
  r
  return matches
next_between l r c = do
  next_char l
  mathced <- lexeme c
  next_char r
  return mathced
next_char x = lexeme $ satisfy (== x)
next_op2 = do
  spaces1
  op <- (many1 $ some "+-*/|&")
    <|> (char '.' >> return ".")
    <|> (string "==" >> return "==")
    <|> (string "=>" >> return "=>")
  return op
next_update_op2 = lexeme $ many1 $ some "+-*/|&:="
next_id = lexeme $ do
  prefix <- some $ az ++ symbols
  remaining <- many $ some (az ++ num ++ symbols)
  return $ prefix : remaining
next_type = (next_between '[' ']' next_type)
            <|> (next_between '(' ')' next_type)
            <|> next_id
next_br = do
  many $ some " \t"
  many1 $ some "\r\n"
next_br1 = next_br >> string "  "
next_br2 = next_br >> string "    "
skip_white_spaces = many $ some " \t\r\n"
eof = do
  many $ some " \t\r\n"
  (string "__comment__\n" >> (many $ satisfy (const True))) <|> (return "")
  Parser $ \s -> if s == ""
    then Hit () s
    else Miss $ "rest: " ++ (show s)


-- make AST
make_func [] ast   = ast
make_func args ast = Func args ast
make_ref "_" = Void
make_ref s = Ref s

-- parser
parse :: String -> Either (String, Env) Env
parse input = case runParser parse_root (trim input ++ "\n") of
  Hit env ""   -> Right env
  Hit env left -> Left ("left: " ++ (show left), env)
  Miss m       -> Left (m, [])
 where
  trim s = reverse $ dropWhile (\x -> elem x " \t\r\n") (reverse s)

parse_root :: Parser Env
parse_root = do
  ret <- sepBy parse_define next_br
  eof
  return ret

parse_define :: Parser (String, AST)
parse_define = do
  name <- next_id
  case name of
    "struct" -> parse_def def_struct
    "enum"   -> parse_def def_enum
    "state"  -> parse_def def_state
    _        -> def_func name
 where
  parse_def f = do
    name <- next_id
    many next_type
    next_char ':'
    fields <- many (next_br1 >> def_line)
    enums <- many (next_br1 >> enum_line name)
    funcs <- many $ do { next_br1; name <- next_id; def_func name }
    return (name, f name fields enums funcs)
  def_struct _ fields _ _ = Func fields $ Struct []
  def_enum _ _ enums _ = Struct enums
  def_state name fields enums funcs = Func fields (Stmt defs [])
    where
      defs = funcs ++ (map emap enums)
      emap (x, _) = (x, Return $ Enum (name ++ "." ++ x) Void)
  def_func name = do
    args <- many next_id
    next_char '='
    top <- parse_top
    return (name, make_func args top)
  def_line = do
    name <- next_id
    type_ <- next_type
    return name
  enum_line prefix = do
    name <- next_id
    fields <- option [] enum_fields
    look_ahead (next_char '\n')
    let tag = prefix ++ "." ++ name
    return (name, Func fields $ Enum tag Void)
  enum_fields = do
    next_char ':'
    many1 (next_br2 >> def_line)

parse_top = (next_br >> (parse_matches <|> parse_stmt))
  <|> parse_exp

parse_matches = Match <$> sepBy1 parse_match next_br
parse_match = do
  next_char '|'
  conds <- many parse_bottom
  next_char '='
  body <- parse_exp
  return $ (conds, body)

parse_stmt = Stmt [] <$> sepBy1 parse_line next_br1
 where
  parse_line :: Parser (String, AST)
  parse_line = parse_assign <|> parse_call
  parse_assign = do
    name <- next_id
    args <- many next_id
    op <- next_update_op2
    ast <- (next_br >> parse_matches) <|> parse_exp
    let short_op = take ((length op) - 1) op
    case op of
      "="  -> return (name, make_func args ast)
      ":=" -> return (name, ast)
      _    -> return (name, Op2 short_op (make_ref name) ast)
  parse_call = do
    ast <- parse_exp
    return ("", ast)

parse_exp = parse_op2
parse_op2 = do
  l <- parse_bottom
  o <- option "" next_op2
  case o of
    "" -> return l
    "=>" -> case l of
      Ref name -> Func [name] <$> (skip_white_spaces >> parse_exp)
    _ -> Op2 o l <$> (skip_white_spaces >> parse_exp)

parse_bottom :: Parser AST
parse_bottom = do
  part <- parse_bool
    <|> parse_str
    <|> parse_num
    <|> parse_list
    <|> parse_ref
    <|> next_between '(' ')' parse_exp
  follow part
 where
  follow :: AST -> Parser AST
  follow part = do
    mark <- option ' ' $ some "(."
    case mark of
      ' ' -> return part
      '(' -> do
        args <- parenthesis noop (char ')') parse_exp
        follow $ Apply part args
      '.' -> do
        name <- many1 $ some $ az ++ num
        args <- option [] $ parenthesis (char '(') (char ')') parse_exp
        follow $ Dot part name args
parse_ref = make_ref <$> next_id
parse_bool = Bool <$> do
  name <- next_id
  case name of
    "true"  -> return True
    "false" -> return False
    label   -> fail $ "miss " ++ label
parse_str = (String <$> (fmap trim1 $ next_between '`' '`' (many $ none "`")))
  <|> (String <$> (fmap unescape $ between (next_char '"') (char '"') (many $ none "\"")))
 where
  trim1 s = reverse $ _trim1 $ reverse $ _trim1 s
  _trim1 ('\n':s) = s
  _trim1 s        = s
  unescape [] = []
  unescape ('\\':('n':xs)) = '\n' : unescape xs
  unescape (x:xs) = x : unescape xs
parse_num = do
  spaces
  n <- many1 (some $ '-' : num)
  (char '.' >> real n) <|> (int n)
 where
  int n = return $ Int (read n :: Int)
  real n = do
    m <- many1 (some $ '-' : num)
    return $ Real (read (n ++ "." ++ m) :: Double)
parse_list = List <$> parenthesis (next_char '[') (char ']') parse_exp



--( Evaluator )-------------------------------------------------------
data Scope = Scope {
    local :: Env
  , global :: Env
  } deriving (Show)
data Ret a = Success { ret :: a, scope :: Scope }
           | Fail { messgage :: String, scope :: Scope }
data Eval a = Eval { runEval :: Scope -> Ret a }

emap m f = Eval $ \e -> case runEval m e of
  Success a e -> f a e
  Fail m e -> Fail m e

instance Functor Eval where
  fmap f m = emap m $ \a e -> Success (f a) e

instance Applicative Eval where
  pure a = Eval $ \s -> Success a s
  l <*> r = do
    f <- l
    a <- r
    return $ f a

instance Monad Eval where
  return = pure
  m >>= f = emap m $ \r e -> runEval (f r) e
  fail m = Eval $ \e -> Fail m e

get_scope :: Eval Scope
get_scope = Eval $ \s -> Success s s

find name = do
  s <- get_scope
  look name $ local s ++ global s

look name table = do
  case lookup name table of
    Just x -> eval x
    Nothing -> fail $ "not found " ++ name ++ " in " ++ (show table)

with_local e env = Eval $ \s -> case runEval e $ s { local = env ++ local s } of
  Success a _ -> Success a s
  Fail m s -> Fail m s

eval :: AST -> Eval AST
eval x@(Void) = return x
eval x@(Int _) = return x
eval x@(Real _) = return x
eval x@(Bool _) = return x
eval x@(String _) = return x
eval (Func [] ast) = return ast
eval x@(Func _ _) = return x
eval x@(Closure _ _) = return x
eval x@(Match _) = return x
eval x@(Struct _) = return x
eval x@(Enum _ _) = return x
eval x@(Return _) = return x
eval x@(Update _ _ _) = return x
eval (List xs) = List <$> mapM eval xs
eval (Op2 op left right) = do
  el <- eval left
  er <- eval right
  f op el er
 where
  f "||" (Bool False) er  = return er
  f "||" (Bool True) _   = return $ Bool True
  f "+" (Int l) (Int r) = return $ Int $ l + r
  f "-" (Int l) (Int r) = return $ Int $ l - r
  f "*" (Int l) (Int r) = return $ Int $ l * r
  f "/" (Int l) (Int r) = return $ Int $ truncate $ (fromIntegral l) / (fromIntegral r)
  f "+" (Real l) (Real r) = return $ Real $ l + r
  f "-" (Real l) (Real r) = return $ Real $ l - r
  f "*" (Real l) (Real r) = return $ Real $ l * r
  f "/" (Real l) (Real r) = return $ Real $ l / r
  f "." (String l) (String r) = return $ String $ l ++ r
  f "++" (List l) (List r) = return $ List $ l ++ r
  f "==" l r = return $ Bool $ l == r
  f op l r = fail $ "fail op: " ++ op ++ "\n- left: " ++ (show l) ++ "\n- right: " ++ (show r)
eval (Dot target name apply_args) = do
  args <- mapM eval apply_args
  ret <- eval target
  case (ret, name, args) of
    ((Struct env), _, _) -> look name env >>= \body -> eval $ Apply body args
    ((Stmt env stmt), _, _) -> look name env >>= \body -> eval $ Stmt env $ ("", Apply body args) : stmt
    ((String s), "length", _) -> return $ Int $ length s
    ((String s), _, _) -> return $ String [s !! (read name :: Int)]
    ((List xs), "map", [func]) -> List <$> (mapM eval $ map (\x -> Apply func [x]) xs)
    ((List xs), "fold", [init, func]) -> fold_monad func init xs
    ((List xs), "join", [String glue]) -> return $ String (p_join glue xs)
    ((List xs), "filter", [func]) -> List <$> (p_filter func xs [])
    ((List s), _, _) -> return $ s !! (read name :: Int)
    ((Enum _ ast), _, _) -> eval (Dot ast name apply_args)
    ((Func _ _), "bind", _) -> return $ Closure args $ ret
    ((Int v), "str", _) -> return $ String $ show v
    _ -> fail $ "not found1 " ++ name ++ " in " ++ (show ret)
 where
   fold_monad :: AST -> AST -> [AST] -> Eval AST
   fold_monad _ ast [] = return ast
   fold_monad func left (right:rest) = do
     ast <- eval $ Apply func [left, right]
     fold_monad func ast rest
   p_filter :: AST -> [AST] -> [AST] -> Eval [AST]
   p_filter _ [] acc = return $ reverse acc
   p_filter f@(Func _ _) (x:xs) acc = do
     ast <- eval $ Apply f [x]
     case ast of
       (Bool True) -> p_filter f xs (x : acc)
       (Bool False) -> p_filter f xs acc
       _ -> fail $ "not bool: " ++ (show ast)
   p_join :: String -> [AST] -> String
   p_join _ [] = ""
   p_join _ [(String s)] = s
   p_join glue ((String l):rest) = l ++ glue ++ (p_join glue rest)
   p_join glue (x:rest) = (show x) ++ glue ++ (p_join glue rest)
eval (Apply target []) = eval target
eval (Apply target apply_args) = do
  args <- mapM eval apply_args
  v <- eval target
  case v of
    Func fargs (Stmt state []) -> return $ Stmt ((zip fargs args) ++ state) []
    Func fargs (Struct fields) -> return $ Struct (zip fargs args)
    Func fargs (Match conds) -> with_local (match conds args) $ zip fargs args
    Func fargs (Enum tag _) -> return $ Enum tag $ Struct (zip fargs args)
    Func fargs body -> with_local (eval body) $ zip fargs args
    Closure binds ast -> eval $ Apply ast $ binds ++ args
    Match conds -> match conds args
    other -> fail $ "invalid apply: " ++ show other ++ " with " ++ show args
 where
  match :: [([AST], AST)] -> [AST] -> Eval AST
  match all_conds args = if all (\x -> (length $ fst x) == (length args)) all_conds
    then _match all_conds
    else fail $ "does not match the number of matching\nconds=" ++ (show $ map fst all_conds) ++ "\nargs = " ++ (show args)
   where
    _match :: [([AST], AST)] -> Eval AST
    _match [] = fail $ "can't match\nconds = " ++ (show $ map fst all_conds) ++ "\n == " ++ (show args) ++ "\n == " ++ (show apply_args) ++ "\nfrom \n" ++ (show target)
    _match ((conds, body):rest) = do
      if conds `equals` args
        then eval body
        else _match rest
  equals [] []         = True
  equals (x:xs) (y:ys) = equal x y && equals xs ys
  equals _ _           = False
  equal (Void) _                     = True
  equal (Dot (Ref x) y _) (Enum z _) = x ++ "." ++ y == z
  equal (Ref "str") (String _)       = True
  equal (Ref "int") (Int _)          = True
  equal (Ref "bool") (Bool _)        = True
  equal (Ref "real") (Real _)        = True
  equal x y                          = x == y

eval (Ref name) = find name

eval (Stmt env lines) = with_local (exec lines) env
 where
  exec :: [(String, AST)] -> Eval AST
  exec [("", ast)] = eval ast
  exec ((line@(_, Func _ _)):lines) = with_local (exec lines) [line]
  exec ((assign, ast):lines) = do
    let local a = do { v <- eval a; with_local (exec lines) [(assign, v)] }
    v <- eval ast
    case v of
      Return ast -> eval ast
      Update name op ast -> local $ case op of
        ":=" -> ast
        _  -> Op2 (tail op) (Ref name) ast
      ast -> local ast
  exec x = fail $ show x

evaluate env ast = runEval (eval ast) Scope { global = env, local = [] }


--( Utility )---------------------------------------------------------
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
fmt (Dot ast name args) = (fmt ast) ++ "." ++ name ++ (show args)
fmt (Apply body args) = (fmt body) ++ "(" ++ (join " " (map fmt args)) ++ ")"
fmt (Stmt env stmt) = "stmt: " ++ (show env) ++ "  " ++ (show stmt) 
fmt (Update name op ast) = name ++ op ++ (fmt ast)
fmt (Return ast) = "return: " ++ (fmt ast)
fmt (Match m) = "match: " ++ (show m)
fmt (Struct fields) = "(" ++ (fmt_env fields) ++ ")"
fmt (Enum tag (Struct [])) = tag
fmt (Enum tag Void) = tag
fmt (Enum tag val) = tag ++ (fmt val)
fmt (Void) = "_"

fmt_env xs = (join "  " (map tie xs))
 where
  tie (k, v) = k ++ ":" ++ (fmt v)

fmt_scope (Scope local global) = fmt_env "local" local
  -- ++ fmt_env "global" global
  -- "global(detail):\n  " ++ (join "\n  " $ map (tie show) global)
 where
  fmt_env title [] = title ++ ": (empty)\n"
  fmt_env title xs = title ++ ":\n  " ++ (join "\n  " (map (tie fmt) xs)) ++ "\n"
  tie f (k, v) = k ++ "\t= " ++ (f v)

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
