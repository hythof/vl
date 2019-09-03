module Parser where

import Debug.Trace (trace)
import AST

parse :: String -> (Env, String)
parse input = case runParser parse_root (Source {original = input, source = input, indentation = 0, line = 1, column = 1}) of
  Just (a, s) -> (a, source s)
  Nothing -> ([], input)

parseAST :: String -> AST
parseAST input = case runParser parse_exp (Source {original = input, source = input, indentation = 0, line = 1, column = 1}) of
  Just (a, s) -> a
  Nothing -> String "ERROR: parse failed"

-- parser combination
parse_root = do
  defines <- many (spaces >> parse_define)
  option () parse_tail_comment
  return $ defines
parse_top = do
  many $ oneOf " \t"
  v <- parse_match <|> parse_block <|> parse_lazy <|> parse_exp <|> (miss "top")
  eol
  return v
parse_bottom = go
  where
    go = do
      bottom <- (next_between "(" ")" parse_exp) <|> parse_value <|> parse_ref
      chain bottom
    chain unit = do
      op <- option ' ' (oneOf "(.")
      switch op unit
    switch '.' unit = do
      name <- token <|> (many1 $ oneOf "0123456789")
      args <- option [] (between (string "(") (spaces >> string ")") read_args1)
      chain $ Call name (unit : args)
    switch '(' unit = do
      args <- many1 (spaces >> parse_exp)
      spaces
      string ")"
      let (Call name []) = unit
      chain $ Call name args
    switch _ unit = return unit
    read_args1 = many1 (spaces >> parse_exp)
-- define
parse_define = go
  where
    go = def_type <|> def_func
    def_func = do
      name <- next_token
      args <- many next_token
      next_string "="
      body <- parse_top
      return $ (name, make_func args body)
    def_type = do
      kind <- next_token
      name <- next_token
      many next_type -- drop generic types
      next_string ":"
      next_br
      body <- indent $ switch kind name
      return $ (name, body)
    switch "enum" name = do
      tags <- indented_lines next_tag
      let methods = map (\(x:xs) -> (x, make_class (name ++ ":" ++ x) xs [])) tags
      return $ make_class name [] methods
    switch "struct" name = do
      props <- indented_lines next_property
      return $ make_class name props []
    switch "flow" name = do
      tags <- indented_lines next_tag
      next_br
      props <- indented_lines next_property
      next_br
      methods <- indented_lines def_func
      let throws = map (\(x:xs) -> (x, make_throw (name ++ ":" ++ x) xs)) tags
      return $ make_class name props (methods ++ throws)
    switch kind name = miss $ "unknown " ++ kind
    next_property = do
      name <- next_token
      next_type
      return name
    next_tag = do
      tag <- next_token
      props <- sepBy next_property (next_string ",")
      return $ tag : props
-- value
parse_value = parse_string <|> parse_float <|> parse_int <|> parse_void <|> parse_bool <|> parse_list
parse_void = (next_string "()") >> (return Void)
parse_bool = ((next_string "true") >> (return (Bool True))) <|>
             ((next_string "false") >> (return (Bool False)))
parse_float = do
  s1 <- next $ many1 $ oneOf "0123456789"
  string "."
  s2 <- next $ many1 $ oneOf "0123456789"
  return $ Float (read (s1 ++ s2) :: Double)
parse_int = do
  s <- next $ many1 $ oneOf "0123456789"
  return $ Int (read s :: Int)
parse_string = (String <$> (fmap (unescape . trim1) $ next_between "`" "`" (many $ noneOf "`")))
  <|> (String <$> (fmap unescape $ between (next_string "\"") (string "\"") (many $ noneOf "\"")))
 where
  trim1 s = reverse $ _trim1 $ reverse $ _trim1 s
  _trim1 ('\r':s) = s
  _trim1 ('\n':s) = s
  _trim1 ('\t':s) = s
  _trim1 (' ':s) = s
  _trim1 s = s
  unescape [] = []
  unescape ('\\':('n':xs)) = '\n' : unescape xs
  unescape (x:xs) = x : unescape xs
parse_list = List <$> (between
  (next_string "[")
  (spaces >> (string "]"))
  (sepBy (spaces >> parse_exp) spaces1))
-- expression
parse_ref = do
  v <- next_token
  return $ Call v []
parse_exp = parse_op2
parse_op2 = go
  where
    go = do
      l <- parse_bottom
      op <- option "" next_op
      case op of
        "" -> return l
        ":=" -> update l
        "=>" -> func l
        "=" -> assign l
        _ -> op2 op l
    func Void = Func [] <$> parse_exp
    func (Call name []) = Func [name] <$> parse_exp
    assign (Call name []) = Assign name <$> parse_exp
    update (Call name []) = Update name <$> parse_exp
    op2 op l = do
      r <- parse_op2
      return $ Call op [l, r]
parse_match = Match <$> many1 go
  where
    go = do
      next_br
      next_string "| "
      conds <- many1 (match_enum <|> parse_bottom <|> match_all)
      next_string "= "
      body <- parse_exp
      return (conds, body)
    match_enum = do
      name1 <- next_token
      string "."
      name2 <- token
      let name = name1 ++ ":" ++ name2
      return $ Call name []
    match_all = do
      next_string "_"
      return Void
parse_block = next_br >> (indent (go <|> miss "block"))
  where
    go = Block <$> indented_lines parse_exp
parse_lazy = next_string "() =>" >> fmap (Func []) parse_block
-- comments
parse_tail_comment = do
  spaces1
  string "__comment__"
  Parser $ \s -> Just ((), s { source = "" })
-- utility
eof = Parser $ \s -> if 0 == (length $ source s) then Just ((), s) else Nothing
eol = (look next_br) <|> (miss "eol")
debug mark = Parser $ \s -> trace ("@ " ++ show mark ++ " | " ++ show s) (return ((), s))
current_indent = Parser $ \s -> return (indentation s, s)

make_func [] body = body
make_func args body = Func args body

make_class name props methods = Class $ (map (\x -> (x, Void)) props) ++ methods ++ [("__name", (String name))]
make_throw name props = Throw $ map (\x -> (x, Void)) props

satisfy f = Parser $ \s -> do
  let src = source s
  guard ((length src) > 0)
  let c = src !! 0
  guard (f c)
  let newLine = (line s) + if c == '\n' then 1 else 0
  let newColumn = if c == '\n' then 1 else (column s) + 1
  Just (c, s { source = drop 1 src, line = newLine, column = newColumn })

look f = Parser $ \s -> case runParser f s of
  Just (a, _) -> Just (a, s)
  Nothing -> Nothing

guard c = if c then Just () else Nothing

l <|> r = Parser $ \s -> case runParser l s of
  Just v -> return v
  _ -> runParser r s

oneOf xs = satisfy (\x -> elem x xs)
noneOf xs = satisfy (\x -> not $ elem x xs)
select [] = Parser $ \_ -> Nothing
select (x:xs) = (string x) <|> (select xs)

option v f = f <|> (return v)

many f = go []
  where
    go acc = option (reverse acc) (rec acc)
    rec acc = do
      v <- f
      go (v : acc)
many1 f = do
  x <- f
  xs <- many f
  return $ x : xs

sepBy f sep = option [] $ sepBy1 f sep
sepBy1 f sep = do
  x <- f
  xs <- many (sep >> f)
  return $ x : xs

next_between l r m = between (next_string l) (next_string r) m
between l r m = do
  l
  ret <- m
  r <|> (miss "between: not terminated")
  return ret

spaces = many $ oneOf " \t\r\n"
spaces1 = many1 $ oneOf " \t\r\n"

next f = (many $ oneOf " \t") >> f

next_string s = next $ string s
string s = go s
  where
    go [] = return s
    go (x:xs) = (satisfy (== x)) >> (go xs)

az = "abcdefghijklmnopqrstuvxwyz"
num = "0123456789"
sym = "_"
next_token = next token
next_type = next (many1 $ oneOf (az ++ num ++ sym ++ "[]"))
token = go
  where
    go = do
      prefix <- oneOf (sym ++ az)
      remaining <- many $ oneOf (sym ++ az ++ num)
      return $ prefix : remaining

next_op = do
    many $ oneOf " \t"
    o <- select op
    spaces
    return o
  where
    op = op2 ++ op1
    op2 = ["<-", ":=", "==", "!=", ">=", "<=", "||", "&&", "=>", "++"]
    op1 = map (\x -> [x]) ".+-*/%|<>="

next_br = do
  next (eof <|> ((oneOf ";\n") >> return ()))
  return ()

indent f = Parser $ \s ->
  case runParser f (s { indentation = 1 + indentation s }) of
    Just (a, ss) -> Just (a, ss { indentation = indentation s })
    Nothing -> Nothing

indented_lines f = fmap concat $ sepBy1 (indented_line f) next_br
indented_line f = do
  n <- current_indent
  string (take (2 * n) (repeat ' '))
  v <- sepBy1 f (next $ oneOf ",;")
  look next_br
  return v

miss message = Parser $ \s ->
  let
    l = line s
    c = column s
    i = indentation s
    code = unlines $ take (min l 3) $ drop (l - 3) $ lines (original s)
    m = take (c - 1) $ repeat ' '
  in error $ "failed parse at `" ++ message ++ "`\n" ++ code ++ m ++ "^ " ++ (show l) ++ ":" ++ (show c) ++ "\nindent: " ++ show i
