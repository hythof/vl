module Parser where

import Debug.Trace (trace)
import AST

parse :: String -> (Env, String)
parse input = case runParser parse_root (Source { source = input, indent = 0}) of
  Just (a, s) -> (a, source s)
  Nothing -> ([], input)

-- parser combination
parse_root = many (spaces >> parse_define)
parse_define = go
  where
    go = do
      name <- next_token
      case name of
        "enum" -> skip_generic_and_mark def_enum
        "struct" -> skip_generic_and_mark def_struct
        "flow" -> skip_generic_and_mark def_flow
        _ -> def_func name
    def_enum = do
      names <- sepBy1 enum_line next_sep
      return $ Struct (map (\(x:xs) -> (x, to_enum x xs)) names)
    def_struct = do
      names <- sepBy1 struct_line next_sep
      return $ Func names Void
    def_flow = do
      names <- sepBy struct_line next_sep
      many next_sep
      throws <- sepBy enum_line next_sep
      let funcs = map (\(x:_) -> (x, Throw x)) throws
      return $ make_func names (Struct funcs)
    def_func name = do
      args <- many next_token
      next_string "="
      body <- parse_top
      return $ (name, make_func args body)
    skip_generic_and_mark f = do
      name <- next_token
      many next_token
      next_string ":"
      many next_br
      ast <- f
      return (name, ast)
    to_struct xs = Struct $ map (\x -> (x, Void)) xs
    struct_line = do
      name <- next_token
      next_type
      return name
    enum_line = do
      x <- next_token
      xs <- sepBy (do { v <- next_token; next_token; return v }) next_sep
      return $ x : xs
    to_enum x [] = Enum x Void
    to_enum x ys = make_func ys (Enum x Void)
parse_top = parse_match <|> parse_steps <|> parse_exp
parse_exp = parse_op2
parse_unit = parse_ref_apply_method <|> parse_value
-- value
parse_value = parse_int <|> parse_void <|> parse_bool
parse_void = (next_string "()") >> (return Void)
parse_bool = ((next_string "true") >> (return (Bool True))) <|>
             ((next_string "false") >> (return (Bool False)))
parse_int = do
  s <- next $ many1 $ oneOf "0123456789"
  return $ Int (read s :: Int)
-- container
parse_list = between_string "[" "]" parse_exp
-- expression
parse_ref = Ref <$> next_token
parse_ref_apply_method = parse_method <|> parse_ref_apply
  where
    parse_method = do
      target <- parse_ref_apply <|> parse_value
      string "."
      name <- next_token
      args <- read_args
      return $ Method target name args
    parse_ref_apply = do
      id <- next_token
      args <- read_args
      return $ make_apply id args
parse_op2 = go
  where
    go = do
      l <- parse_unit
      op <- option "" next_op
      ret <- consume l op
      return ret
    consume l op = case op of
      "" -> return l
      ":=" -> do
        let (Ref name) = l
        e <- parse_exp
        return $ Assign name e
      _ -> do
        r <- parse_op2
        return $ Op2 op l r
parse_match = Match <$> many1 go
  where
    go = do
      next_br
      next_string "|"
      cond <- parse_value <|> parse_ref
      next_string "="
      body <- parse_exp
      return (cond, body)
parse_steps = Steps <$> many1 go
  where
    go = do
      next_br1
      read_return <|> read_throw <|> read_assign <|> read_apply
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
parse_func = do
  arg <- next_token
  next_string "=>"
  body <- parse_top
  return $ Func [arg] body

-- utility
debug mark = Parser $ \s -> trace ("@ " ++ show mark ++ " | " ++ show s) (return ((), s))
read_args = option [] (between (string "(") (next_string ")") (many1 parse_exp))
make_apply "true" [] = Bool True
make_apply "false" [] = Bool False
make_apply id [] = Ref id
make_apply id args = Apply (Ref id) args
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

between_string l r m = between (next_string l) (next_string r) m
between l r m = do
  l
  ret <- m
  r
  return ret

spaces = many $ oneOf " \t\r\n"
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
    op2 = ["==", "!=", ">=", "<=", "||", "&&"]
    op1 = map (\x -> [x]) ".+-*/%|<>"

next_sep = next $ string ","
next_br = next $ oneOf ";\n"
next_br1 = next_br >> string "  "
