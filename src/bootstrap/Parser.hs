module Parser(parse, program, ast_exp, defines) where
import           AST
import           Control.Applicative (Alternative, Applicative, empty, pure,
                                      (*>), (<*>), (<|>))
import           Debug.Trace         (trace)
import           ParserLib

parse :: Parser a -> String -> Either String a
parse p s = case runParser p (Source s 0) of
    Just (_, v) -> Right v
    Nothing -> Left "fail parse"

program :: Parser [(String, AST)]
program = defines

-- AST
defines = sepBy define br
define = def_func <|> def_type <|> def_const
def_func = do
  name <- ref
  args <- ast_args
  op_define
  value <- ast_exp
  return $ (name, if length args == 0 then value else Func args value)
def_type = do
  name <- ref
  ast_args -- no support type arguments
  body <- ast_algebra <|> ast_struct
  return $ (name, body)
def_const = do
  name <- ref
  value <- ast_exp
  return $ (name, value)

ast_exp = ast_op1
  <|> ast_op2
  <|> ast_func
  <|> ast_bool
  <|> ast_call
  <|> ast_atom
  <|> (fail "no exp")

ast_op1 = do
  op <- op1
  x <- ast_atom
  return $ Call [Ref [[op]], x]

ast_op2 = do
  l <- ast_atom
  op <- op2
  r <- ast_atom
  return $ Call [Ref [op], l, r]

ast_func = do
  xs <- ast_args1
  arrow
  body <- ast_exp
  return $ Func xs body

ast_atom = lexeme $ ast_sequence
  <|> ast_bracket
  <|> ast_val
  <|> ast_ref
  <|> (fail "no atom")

ast_val = ast_list
  <|> ast_algebra
  <|> ast_struct
  <|> ast_char
  <|> ast_string
  <|> ast_bool
  <|> ast_real
  <|> ast_int
  <|> (fail "no value")

ast_bracket = within '(' ')' ast_exp

ast_sequence = between (lexeme $ string "(do") (lexeme $ char ')') seq
 where
  seq = lift Seq (sepBy (lexeme $ ast_assign <|> ast_exp) br)
  ast_assign = lift2 Assign ref (spaces >> string "<=" >> spaces >> ast_exp)

ast_call = do
  f <- ast_ref
  args <- many $ lexeme ast_exp
  return $ if length args == 0 then f else Call $ f : args

ast_args    = many ref
ast_args1   = many1 ref
ast_ref     = lift Ref $ lexeme $ sepBy1 token (char '.') 
ast_list    = lift List $ within '[' ']' (sepBy ast_exp br)
ast_algebra = lift Recursive $ within '{' '}' (sepBy1 (many1 $ ref) (lexeme $ char '|'))
ast_struct  = lift Struct $ within '{' '}' defines
ast_char    = lift Char $ within '\'' '\'' (noneOf ['\''])
ast_string  = lift String $ within '"' '"' (many $ noneOf ['"'])
ast_bool    = lift (\x -> Bool $ x == "true") (string "true" <|> string "false")
ast_real    = lift3 (\a b c -> Real $ (read (a ++ [b] ++ c))) nat (char '.') nat
ast_int     = lift Int (fmap read nat)

-- with lexeme
ref = lexeme $ token
op1 = lexeme $ oneOf "+-!~"
op2 = lexeme (string "<="
  <|> string ">="
  <|> string "=="
  <|> string "<=>"
  <|> string ">>"
  <|> string "<<"
  <|> string "||"
  <|> string "//"
  <|> string "**"
  <|> string "&&"
  <|> (fmap (\x -> [x]) $ oneOf "+-*/%><|&"))
op_define = lexeme $ oneOf "=:"
arrow = lexeme $ string "=>"

-- bottom
alpha     = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
nat       = many1 $ oneOf "0123456789"
token     = lift2 (:) alpha (many $ oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
spaces    = many $ oneOf " \t"
spaces1   = many1 $ oneOf " \t"
within l r p = between (rtrim $ char l) (ltrim $ char r) p
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
