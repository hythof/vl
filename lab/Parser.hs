module Parser where

import Debug.Trace (trace)
import AST

parse :: String -> (Env, String)
parse input = case runParser parse_root (Source {source = input}) of
  Just (a, s) -> (a, source s)
  Nothing -> ([], input)

-- parser combination
parse_root = many (spaces >> parse_define)
parse_top = parse_match <|> parse_block <|> parse_exp
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
      chain $ Method unit name args
    switch '(' unit = do
      args <- many1 parse_exp
      next_string ")"
      chain $ Apply unit args
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
      body <- next_brackets (switch kind)
      return $ (name, body)
    switch "enum" = do
      tags <- many (next_br >> next_tag)
      return $ Struct (map (\(x:xs) -> (x, to_enum x xs)) tags)
    switch "struct" = do
      props <- many (next_br >> next_property)
      return $ Func props Void
    switch "class" = do
      props <- many (next_br >> next_property)
      tags <- many (next_br >> next_tag)
      methods <- option [] next_methods
      return $ make_func props (Struct $ methods ++ map to_throw tags)
    switch kind = error $ "unsupported parse " ++ kind
    next_methods = do
      spaces
      next_string "}"
      spaces
      next_string "method "
      many1 next_token
      next_string "{"
      spaces
      sepBy1 def_func next_br
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
parse_value = parse_string <|> parse_int <|> parse_void <|> parse_bool
parse_void = (next_string "()") >> (return Void)
parse_bool = ((next_string "true") >> (return (Bool True))) <|>
             ((next_string "false") >> (return (Bool False)))
parse_int = do
  s <- next $ many1 $ oneOf "0123456789"
  return $ Int (read s :: Int)
parse_string = String <$> between (next_string "\"") (string "\"") (many $ noneOf "\"")
-- container
parse_list = next_between "[" "]" parse_exp
-- expression
parse_ref = Ref <$> next_token
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
        let (Ref name) = l
        body <- parse_exp
        return $ Func [name] body
      ":=" -> do
        let (Ref name) = l
        e <- parse_exp
        return $ Assign name e
      _ -> do
        r <- go
        return $ Op2 op l r
parse_match = Match <$> many1 go
  where
    go = do
      next_br
      next_string "|"
      pattern <- parse_pattern
      next_string "="
      body <- parse_exp
      return (pattern, body)
    parse_pattern = parse_enum_pattern
    parse_enum_pattern = EnumPattern <$> next_token
parse_block = Block <$> next_brackets go
  where
    go = sepBy1 (spaces >> step) next_br
    step = read_return <|> read_throw <|> read_assign <|> read_apply
    read_return = (next_string "return ") >> (Return <$> parse_exp)
    read_throw = (next_string "throw ") >> (Throw <$> next_token)
    read_assign = do
      name <- next_token
      args <- many next_token
      next $ select ["=", ":="]
      body <- read_apply
      return $ Assign name (make_func args body)
    read_apply = do
      target <- parse_exp
      args <- many parse_exp
      return $ if (length args) == 0 then target else Apply target args

-- utility
bug = Parser $ \s -> error $ "BUG\n" ++ show s
debug mark = Parser $ \s -> trace ("@ " ++ show mark ++ " | " ++ show s) (return ((), s))
make_func [] body = body
make_func args body = Func args body

satisfy f = Parser $ \s -> do
  let src = source s
  guard ((length src) > 0)
  let c = src !! 0
  guard (f c)
  Just (c, s { source = drop 1 src })

look f = Parser $ \s -> case runParser f s of
  Just (a, _) -> Just (True, s)
  Nothing -> Just (False, s)

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

next_brackets m = between (next_string "{") (spaces >> (string "}")) m
next_between l r m = between (next_string l) (next_string r) m
between l r m = do
  l
  ret <- m
  r <|> bug
  return ret

spaces = many $ oneOf " \t\r\n"
spaces1 = (oneOf " \t\r\n") >> spaces
next f = (many $ oneOf " \t") >> f

next_string s = next $ string s
string s = go s
  where
    go [] = return s
    go (x:xs) = (satisfy (== x)) >> (go xs)

next_token = next token
next_type = next token
token = go
  where
    go = do
      prefix <- oneOf (sym ++ az)
      remaining <- many $ oneOf (sym ++ az ++ num)
      return $ prefix : remaining
    az = "abcdefghijklmnopqrstuvxwyz"
    num = "0123456789"
    sym = "_"

next_op = next $ select op
  where
    op = op2 ++ op1
    op2 = ["==", "!=", ">=", "<=", "||", "&&", "=>"]
    op1 = map (\x -> [x]) ".+-*/%|<>"

next_br = next $ oneOf ";\n"
