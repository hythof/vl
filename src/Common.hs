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
  | Dot AST String [AST]
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
next_between l r c = do
  next_char l
  hit <- lexeme c
  next_char r
  return hit
next_char x = lexeme $ satisfy (== x)
next_op2 = do
  spaces1
  op <- (many1 $ some "+-*/|&") <|> (char '.' >> return ".")
  spaces1
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

-- make AST
make_func [] ast   = ast
make_func args ast = Func args ast
make_ref "_" = Void
make_ref s = Ref s

-- parser
parse :: String -> Env
parse input = case runParser parse_root (trim input) of
  Hit env ""   -> env
  Hit env left -> env ++ [("err", String $ "left: " ++ left)]
  Miss m       -> [("err", String m)]
 where
  trim s = reverse $ dropWhile (\x -> elem x " \t\r\n") (reverse s)

parse_root :: Parser Env
parse_root = sepBy parse_define next_br

parse_define :: Parser (String, AST)
parse_define = do
  name <- next_id
  case name of
    "struct" -> def_struct
    "enum"   -> def_enum
    "state"  -> def_state
    _        -> def_func name
 where
  def_struct = do
    name <- next_id
    many next_type -- TODO: generics
    next_char ':'
    fields <- many1 (next_br1 >> def_line)
    return (name, TypeStruct fields)
  def_enum = do
    name <- next_id
    many next_type -- TODO: generics
    next_char ':'
    fields <- many1 (next_br1 >> enum_line name)
    return (name, Struct fields)
  def_state = do
    name <- next_id
    many next_type -- TODO: generics
    next_char ':'
    fields <- many1 (next_br1 >> def_line)
    exception_names <- many $ do { next_br1; name <- next_id; look_ahead next_br; return name }
    let exceptions = map (\x -> (x, Return $ Enum (name ++ "." ++ x) $ String "_error")) exception_names
    funcs <- many $ do { next_br1; name <- next_id; def_func name }
    return (name, TypeState fields $ exceptions ++ funcs)
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
    let tag = prefix ++ "." ++ name
    return (name, TypeEnum tag fields)
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

parse_stmt = Stmt <$> sepBy1 parse_line next_br1
 where
  parse_line :: Parser (String, AST)
  parse_line = parse_assign <|> parse_call
  parse_assign = do
    name <- next_id
    op <- next_update_op2
    ast <- parse_exp
    let short_op = take ((length op) - 1) op
    case op of
      "="  -> return (name, ast)
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
    _ -> do
      r <- parse_exp
      return $ Op2 o l r

parse_bottom :: Parser AST
parse_bottom = do
  part <- parse_bool
    <|> parse_str
    <|> parse_num
    <|> parse_list
    <|> parse_ref
  follow part
 where
  follow :: AST -> Parser AST
  follow part = do
    mark <- option ' ' $ some "(."
    case mark of
      ' ' -> return part
      '(' -> do { args <- many parse_bottom; next_char ')'; follow $ Apply part args }
      '.' -> do
        name <- many1 $ some $ az ++ num
        args <- option [] $ between (char '(') (next_char ')') $ many parse_bottom
        follow $ Dot part name args
parse_ref = make_ref <$> next_id
parse_bool = Bool <$> do
  name <- next_id
  case name of
    "true"  -> return True
    "false" -> return False
    label   -> fail $ "miss " ++ label
parse_str = (String <$> (fmap trim1 $ next_between '`' '`' (many $ none "`")))
  <|> (String <$> next_between '"' '"' (many $ none "\""))
 where
  trim1 s = reverse $ _trim1 $ reverse $ _trim1 s
  _trim1 ('\n':s) = s
  _trim1 s        = s
parse_num = do
  spaces
  n <- many1 (some $ '-' : num)
  (char '.' >> real n) <|> (int n)
 where
  int n = return $ Int (read n :: Int)
  real n = do
    m <- many1 (some $ '-' : num)
    return $ Real (read (n ++ "." ++ m) :: Double)
parse_list = List <$> (next_between '[' ']' (many parse_bottom))



--( Evaluator )-------------------------------------------------------
data Scope = Scope {
    local :: Env
  , global :: Env
  , near :: Env
  } deriving (Show)
data Ret a = Ok { ret :: a, scope :: Scope }
           | Fail { messgage :: String, scope :: Scope }
data Eval a = Eval { runEval :: Scope -> Ret a }

emap m f = Eval $ \e -> case runEval m e of
  Ok a e -> f a e
  Fail m e -> Fail m e
emap2 l r f = Eval $ \e -> f (runEval l e) (runEval r e)

instance Functor Eval where
  fmap f m = emap m $ \a e -> Ok (f a) e

instance Applicative Eval where
  pure a = Eval $ \e -> Ok a e
  l <*> r = emap2 l r $ \m n -> n {ret = (ret m) (ret n)}

instance Monad Eval where
  return = pure
  m >>= f = emap m $ \r e -> runEval (f r) e
  fail m = Eval $ \e -> Fail m e


get_scope :: Eval Scope
get_scope = Eval $ \s -> Ok s s

find name = do
  s <- get_scope
  look name $ local s ++ near s ++ global s

look name table = do
  case lookup name table of
    Just x -> ev x
    Nothing -> fail $ "not found" ++ name ++ " in " ++ (show table)

puts_local kvs = Eval $ \s -> Ok Void $ s {local = kvs ++ local s}
puts_global kvs = Eval $ \s -> Ok Void $ s {global = kvs ++ global s}
puts_near kvs = Eval $ \s -> Ok Void $ s {near = kvs ++ near s}

ev :: AST -> Eval AST
ev x@(Int _) = return x
ev x@(Real _) = return x
ev x@(Bool _) = return x
ev x@(String _) = return x
ev (Func [] ast) = return ast
ev x@(Func _ _) = return x
ev x@(Match _) = return x
ev x@(TypeStruct _) = return x
ev (TypeEnum tag []) = return $ Enum tag $ Struct []
ev x@(TypeEnum _ _) = return x
ev x@(TypeState _ _) = return x
ev x@(Struct _) = return x
ev x@(Enum _ _) = return x
ev x@(Return _) = return x
ev x@(Update _ _ _) = return x
ev (List xs) = List <$> mapM ev xs
ev (Op2 op left right) = do
  el <- ev left
  er <- ev right
  return $ f op el er
 where
  f "||" (Bool False) er  = er
  f "||" (Bool True) _   = Bool True
  f "+" (Int l) (Int r) = Int $ l + r
  f "-" (Int l) (Int r) = Int $ l - r
  f "*" (Int l) (Int r) = Int $ l * r
  f "/" (Int l) (Int r) = Int $ truncate $ (fromIntegral l) / (fromIntegral r)
  f "+" (Real l) (Real r) = Real $ l + r
  f "-" (Real l) (Real r) = Real $ l - r
  f "*" (Real l) (Real r) = Real $ l * r
  f "/" (Real l) (Real r) = Real $ l / r
  f "." (String l) (String r) = String $ l ++ r
  f "++" (List l) (List r) = List $ l ++ r
  f op l r = Error $ "fail op: " ++ op ++ "\n- left: " ++ (show l) ++ "\n- right: " ++ (show r)
ev (Dot target name apply_args) = do
  args <- mapM ev apply_args
  ret <- ev target
  case ret of
    (Struct fields) -> do
      body <- look name fields
      ev $ Apply body args
    (String s) -> case name of
      "length" -> return $ Int $ length s
      _ -> return $ String [s !! (read name :: Int)]
    ast -> fail $ "not found1 " ++ name ++ " in " ++ (show ast)
ev (Apply target apply_args) = do
  args <- mapM ev apply_args
  v <- ev target
  case v of
    Func fargs (Match conds) -> do
      puts_local (zip fargs $ map (\(Enum _ v) -> v) args)
      match conds
    Func fargs body -> do
      mapM_ ev apply_args
      puts_local (zip fargs args)
      ev body
    TypeStruct fields -> do
      let env = zip fields args
      puts_near env
      return $ Struct env
    TypeEnum tag fields -> do
      let env = zip fields args
      puts_near env
      return $ Enum tag $ Struct env
    TypeState fields state -> do
      let env = (zip fields args) ++ state
      puts_near env
      return $ Struct env
    Match conds -> match conds
    other -> return other
 where
  match :: [([AST], AST)] -> Eval AST
  match all_conds = _match all_conds
   where
    _match :: [([AST], AST)] -> Eval AST
    _match [] = fail $ "can't match " ++ (show all_conds) ++ " == " ++ (show apply_args) ++ " from " ++ (show target)
    _match ((conds, body):rest) = do
      args <- mapM ev apply_args
      if conds `equals` args
        then ev body
        else _match rest
  equals [] []         = True
  equals (x:xs) (y:ys) = equal x y && equals xs ys
  equals _ _           = False
  equal (Void) _                         = True
  equal (Dot (Ref x) y _) (TypeEnum z _) = x ++ "." ++ y == z
  equal (Dot (Ref x) y _) (Enum z _)     = x ++ "." ++ y == z
  equal x y                              = x == y

ev (Ref name) = find name

ev (Stmt lines) = exec lines
 where
  exec :: [(String, AST)] -> Eval AST
  exec [("", ast)] = ev ast
  exec ((assign, ast):lines) = do
    v <- ev ast
    case v of
      Return ast -> ev ast
      Update name op ast -> case op of
        ":=" -> do
          v <- ev ast
          puts_local [(assign, v)]
          exec lines
        _  -> do
          left <- ev $ Ref name
          let right = ast
          let op2 = take ((length op) - 1) op
          v <- ev $ Op2 op2 left right
          puts_local [(assign, v)]
          exec lines
       where
      ast -> do
        v <- ev ast
        puts_local [(assign, v)]
        exec lines
ev x@(Error _) = return x
ev ast = error $ "yet: '" ++ (show ast) ++ "'"

eval env ast = case runEval (ev ast) Scope { global = env, local = [], near = [] } of
  Ok a e -> a
  Fail m e -> Error m



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
fmt (Dot ast name args) = (fmt ast) ++ "." ++ name ++ (show args)
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
