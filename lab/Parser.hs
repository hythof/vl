module Parser where

import Debug.Trace (trace)
import AST

parse :: String -> (Env, String)
parse input = case runParser parse_root (Source {source = input, indentation = 0}) of
  Just (a, s) -> (a, source s)
  Nothing -> ([], input)

-- parser combination
parse_root = do
  defines <- many (spaces >> parse_define)
  option () parse_tail_comment
  return $ defines
parse_top = do
  many $ oneOf " \t"
  v <- parse_match <|> parse_block <|> parse_exp <|> (bug "top")
  eol
  return v
parse_exp = parse_op2
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
      args <- option [] (between (string "(") (next_string ")") (many1 parse_exp))
      chain $ Apply name (unit : args)
    switch '(' unit = do
      args <- many1 (spaces >> parse_exp)
      spaces
      string ")"
      let (Apply name []) = unit
      chain $ Apply name args
    switch _ unit = return unit
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
      body <- indent $ switch kind
      return $ (name, body)
    switch "enum" = do
      tags <- indented_lines next_tag
      return $ Struct (map (\(x:xs) -> (x, to_enum x xs)) tags)
    switch "struct" = do
      props <- indented_lines next_property
      return $ Func props Void
    switch "flow" = do
      tags <- indented_lines next_tag
      next_br
      props <- indented_lines next_property
      next_br
      methods <- indented_lines def_func
      let env = methods ++ map to_throw tags
      return $ Func props $ Struct ((map (\name -> (name, Void)) props) ++ env)
    switch kind = error $ "unsupported parse " ++ kind
    to_enum x [] = Enum x Void
    to_enum x ys = make_func ys (Enum x Void)
    to_throw (x:_) = (x, Throw x)
    next_property = do
      name <- next_token
      next_type
      return name
    next_tag = do
      tag <- next_token
      props <- sepBy next_property (next_string ",")
      return $ tag : props
-- value
parse_value = parse_string <|> parse_int <|> parse_void <|> parse_bool <|> parse_list
parse_void = (next_string "()") >> (return Void)
parse_bool = ((next_string "true") >> (return (Bool True))) <|>
             ((next_string "false") >> (return (Bool False)))
parse_int = do
  s <- next $ many1 $ oneOf "0123456789"
  return $ Int (read s :: Int)
parse_string = (String <$> (fmap trim1 $ next_between "`" "`" (many $ noneOf "`")))
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
  return $ Apply v []
parse_op2 = go
  where
    go = do
      l <- parse_bottom
      op <- option "" next_op
      ret <- consume l op
      return ret
    consume l op = case op of
      "" -> return l
      "=>" -> do
        let (Apply name []) = l
        body <- parse_exp
        return $ Func [name] body
      _ -> do
        r <- go
        return $ Apply op [l, r]
parse_match = Match <$> many1 go
  where
    go = do
      next_br
      next_string "|"
      conds <- many1 parse_exp
      next_string "="
      body <- parse_exp
      return (conds, body)
parse_block = Block <$> (next_br >> (indent go))
  where
    go = indented_lines step
    step = read_throw <|> read_assign <|> parse_exp
    read_throw = (next_string "throw ") >> (Throw <$> next_token)
    read_assign = do
      name <- next_token
      args <- many next_token
      op <- next $ select ["=", ":=", "<-"]
      body <- parse_top
      return $ case op of
        "=" -> Define name (make_func args body)
        ":=" -> Update name (make_func args body)
        "<-" -> Assign name (make_func args body)
-- comments
parse_tail_comment = do
  spaces1
  string "__comment__"
  Parser $ \s -> Just ((), s { source = "" })
-- utility
bug message = Parser $ \s -> error $ "BUG " ++ message ++ " \n" ++ show s
eof = Parser $ \s -> if 0 == (length $ source s) then Just ((), s) else Nothing
eol = (look next_br) <|> (bug "eol")
debug mark = Parser $ \s -> trace ("@ " ++ show mark ++ " | " ++ show s) (return ((), s))
current_indent = Parser $ \s -> return (indentation s, s)

make_func [] body = body
make_func args body = Func args body

satisfy f = Parser $ \s -> do
  let src = source s
  guard ((length src) > 0)
  let c = src !! 0
  guard (f c)
  Just (c, s { source = drop 1 src })

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
  r <|> (bug "between")
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

next_op = next $ select op
  where
    op = op2 ++ op1
    op2 = ["==", "!=", ">=", "<=", "||", "&&", "=>", "++"]
    op1 = map (\x -> [x]) ".+-*/%|<>"

next_br = do
  next (eof <|> ((oneOf ";\n") >> return ()))
  return ()

indent f = Parser $ \s ->
  case runParser f (s { indentation = 1 + indentation s }) of
    Just (a, ss) -> Just (a, ss { indentation = indentation s })
    Nothing -> Nothing

indented_lines f = fmap concat $ sepBy1 (indented_line f) next_br
indented_line f = do
  indent <- current_indent
  string (take (2 * indent) (repeat ' '))
  v <- sepBy1 f (next $ oneOf ",;")
  look next_br
  return v
