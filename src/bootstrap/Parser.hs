module Parser(parse, program, ast_exp) where
import           AST
import           Control.Applicative (Alternative, Applicative, empty, pure,
                                      (*>), (<*>), (<|>))
import           Debug.Trace         (trace)
import           ParserLib

parse :: Parser a -> String -> Either String a
parse p s = case runParser p (Source s 0) of
    Just (_, v) -> Right v
    Nothing -> Left "fail parse"

program :: Parser [AST]
program = ast_top


-- AST
ast_top = sepBy ast_declare br

ast_declare = ast_define
          <|> ast_comment

ast_define = do
  reference <- ref
  arguments <- ast_args
  delimiter <- op_define
  body <- ast_unit
  return (if delimiter == ':'
    then Type (reference !! 0) arguments body
    else Def reference arguments body)

ast_comment = do
  char '#'
  x <- many $ noneOf "\r\n"
  return $ Comment x

ast_exp = ast_op1
  <|> ast_op2
  <|> ast_func
  <|> ast_unit
  <|> (fail "miss match exp")

ast_unit = lexeme $ ast_bracket
   <|> ast_val
   <|> ast_ref

ast_bracket = group '(' ')' ast_exp

ast_val = ast_list
  <|> ast_struct
  <|> ast_char
  <|> ast_string
  <|> ast_bool
  <|> ast_real
  <|> ast_int
  <|> (fail "miss match")

ast_op1 = do
  op <- op1
  x <- ast_unit
  return $ Call [Ref [[op]], x]

ast_op2 = do
  l <- ast_unit
  op <- op2
  r <- ast_unit
  return $ Call [Ref [op], l, r]

ast_ref = do
  x <- Ref <$> ref
  xs <- many ast_unit
  return $ if length xs == 0 then x else Call $ x : xs

ast_func = do
  xs <- ast_args1
  arrow
  body <- ast_exp
  return $ Func xs body

ast_arg = ast_val
      <|> Ref <$> ref
ast_args = lexeme $ sepBy ast_arg spaces1
ast_args1 = lexeme $ sepBy1 ast_arg spaces1
ast_list   = lift List $ group '[' ']' (sepBy ast_unit br)
ast_struct = lift Struct $ group '{' '}' (sepBy ast_define br)
ast_char   = lift Char $ group '\'' '\'' (noneOf ['\''])
ast_string = lift String $ group '"' '"' (many $ noneOf ['"'])
ast_bool   = lift (\x -> Bool $ x == "true") (string "true" <|> string "false")
ast_real   = lift3 (\a b c -> Real $ (read (a ++ [b] ++ c))) nat (char '.') nat
ast_int    = lift Int (fmap read nat)

-- lexeme
ref = lexeme $ sepBy1 name (char '.')
op1 = lexeme $ oneOf "+-!~"
op2 = lexeme (
      (fmap (\x -> [x]) $ oneOf "+-*/><")
  <|> string "<="
  <|> string ">="
  <|> string "=="
  <|> string "<=>"
  <|> string ">>"
  <|> string "<<"
  <|> string "||"
  <|> string "&&")
op_define = lexeme $ oneOf "=:"
arrow = lexeme $ string "=>"

-- utility
alpha     = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
nat       = many1 $ oneOf "0123456789"
ops       = string "=="
        <|> string "//"
        <|> string "||"
        <|> string "&&"
        <|> string "**"
        <|> string ">>"
        <|> string "<<"
        <|> (fmap (\x -> [x]) $ oneOf "+-*/%|&<>")
name      = lift2 (:) alpha (many $ oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
spaces    = many $ oneOf " \t"
spaces1   = many1 $ oneOf " \t"
group l r p = between (rtrim $ char l) (ltrim $ char r) p
  where
    ltrim f = white_space >> f
    rtrim f = f >> white_space
    white_space = many $ oneOf " \r\n\t"
br = many1 $ spaces >> (many1 $ oneOf ";\r\n") >> spaces

lift :: Monad f => (a -> b) -> f a -> f b
lift f m = do
    v <- m
    return $ f v

lift2 :: Monad f => (a -> b -> c) -> f a -> f b -> f c
lift2 f m1 m2 = do
    v1 <- m1
    v2 <- m2
    return $ f v1 v2

lift3 :: Monad f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift3 f m1 m2 m3 = do
    v1 <- m1
    v2 <- m2
    v3 <- m3
    return $ f v1 v2 v3
