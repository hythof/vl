module Parser where

import Base
import Control.Applicative ((<|>))
import Control.Monad (guard)

--( Parse AST )----------------------------------------------------------------
parse :: String -> Maybe ([AST], Source)
parse s = runParser parse_top $ Source s 0 (length s)
parse_top = sep_by1 parse_line read_br1
parse_line = parse_type <|> parse_def <|> parse_exp
parse_type = do
  name <- read_id
  args <- many (read_spaces1 >> read_id)
  read_spaces
  defs <- between (char '{' >> read_separator) (read_separator >> char '}') (sep_by parse_def read_br1)
  return $ Struct name (map (\(def@(Def name args lines)) -> (name, def)) defs)
parse_def = do
  name <- read_id
  args <- many (read_spaces1 >> read_id)
  read_spaces
  char '='
  read_spaces
  lines <- parse_stmt <|> (parse_exp >>= \exp -> return [exp])
  return $ Def name args lines
parse_stmt = between (char '{' >> read_separator) (read_separator >> char '}') (sep_by parse_line read_br1)
parse_exp :: Parser AST
parse_exp = go
  where
    go :: Parser AST
    go = do
      left <- parse_unit
      (exp_op_remaining left) <|> (return left)
    exp_op_remaining :: AST -> Parser AST
    exp_op_remaining left = do
      read_spaces
      op <- read_op
      read_spaces
      right <- parse_exp
      return $ Call op [left, right]
parse_unit :: Parser AST
parse_unit = go
  where
    go = do
      x <- parenthis <|> parse_string <|> parse_int <|> parse_bool <|> parse_call
      sub_part x
    sub_part x = nest_part x <|> (return x)
    nest_part x = do
      char '.'
      name <- read_id <|> error "Invalid method calling format"
      argv <- between (char '(' >> read_spaces) (read_spaces >> char ')') (sep_by parse_unit read_spaces1) <|> (return [])
      sub_part $ Call name (x : argv)
    parenthis = between (char '(') (char ')') parse_exp
    parse_string = do
      s <- between (char '"') (char '"') (many $ satisfy (\c -> c /= '"'))
      return $ String s
    parse_int = do
      s <- many1 (satisfy ((flip elem) "0123456789"))
      return $ I64 (read s :: Int)
    parse_bool = do
      s <- string "true" <|> string "false"
      return $ Bool (s == "true")
    parse_call = do
      name <- read_id
      argv <- between (char '(' >> read_spaces) (read_spaces >> char ')') (sep_by parse_unit read_spaces1) <|> (return [])
      return $ Call name argv

--( Read String )---------------------------------------------------------------
read_op :: Parser String
read_op = op2 <|> op1
  where
    op1 = satisfy ((flip elem) "+-*/%") >>= \c -> return [c]
    op2 = try_op2 all_parse_ops
    try_op2 [] = Parser $ \_ -> Nothing
    try_op2 (x:xs) = (string x) <|> try_op2 xs
read_id :: Parser String
read_id = do
  xs <- many1 $ satisfy ((flip elem) "abcdefghijklmnopqrstuvwxyz_")
  ys <- many $ satisfy ((flip elem) "abcdefghijklmnopqrstuvwxyz_0123456789")
  return $ xs ++ ys
read_spaces :: Parser String
read_spaces = many $ satisfy ((flip elem) "\t ")
read_spaces1 :: Parser String
read_spaces1 = many1 $ satisfy ((flip elem) "\t ")
read_separator :: Parser String
read_separator = many $ satisfy ((flip elem) "\n\t ")
read_br1 :: Parser ()
read_br1 = do
  read_spaces
  char '\n'
  many $ satisfy ((flip elem) "\n\t ")
  return ()

--( Parser combinators )--------------------------------------------------------
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> do
  guard $ pos s < len s
  let c = (src s) !! (pos s)
  guard $ f c
  let br = c == '\n'
  return (c, s { pos = 1 + pos s })
many :: Parser a -> Parser [a]
many f = many_acc f []
many1 :: Parser a -> Parser [a]
many1 f = do
  x <- f
  xs <- many f
  return $ x : xs
many_acc :: Parser a -> [a] -> Parser [a]
many_acc f acc = (do
  x <- f
  many_acc f (x : acc)
  ) <|> (return $ reverse acc)
sep_by :: Parser a -> Parser b -> Parser [a]
sep_by body sep = (sep_by1 body sep) <|> return []
sep_by1 :: Parser a -> Parser b -> Parser [a]
sep_by1 body sep = do
  x <- body
  xs <- many (sep >> body)
  return $ x : xs
char :: Char -> Parser Char
char c = satisfy (== c)
string :: String -> Parser String
string target = Parser $ \s -> do
  guard $ target == take (length target) (drop (pos s) (src s))
  return (target, s { pos = length target + pos s })
between l r c = do
  l
  v <- c <|> error "failed in between on the center"
  r <|> error "faield in between on the right"
  return $ v
