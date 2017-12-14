module Parser(parse, file, top) where
import           AST
import           Control.Applicative (Alternative, Applicative, empty, pure,
                                      (*>), (<*>), (<|>))
import           Debug.Trace         (trace)
import           ParserLib

parse :: Parser a -> String -> Either String a
file       :: Parser [(String, AST)]
top        :: Parser AST
expression :: Parser AST
factor     :: Parser AST
value      :: Parser AST
ast_int    :: Parser AST
ast_real   :: Parser AST
ast_bool   :: Parser AST
ast_list   :: Parser AST
ast_string :: Parser AST
ast_struct :: Parser AST
ast_func   :: Parser AST
ast_apply  :: Parser AST
ast_if     :: Parser AST

parse p s = case runParser p (Source s 0) of
    Just (_, v) -> Right v
    Nothing -> Left "fail parse"

-- parse tree
file = sepBy define separator
top  = expression
expression = operator
factor = inner '(' ')' expression
  <|> value
value = ast_list
    <|> ast_struct
    <|> ast_string
    <|> ast_if
    <|> ast_bool
    <|> ast_real
    <|> ast_int
    <|> ast_func
    <|> (spaces >> char '=' >> (notFollowedBy $ char '=') >> spaces >> top)
    <|> ast_apply
    <|> (fail "miss match")

-- value
ast_int    = lift Int (fmap read nat)
ast_real   = lift3 (\a b c -> Real $ (read (a ++ [b] ++ c))) nat (char '.') nat
ast_bool   = lift (\x -> Bool $ x == 'T') (oneOf "TF")
ast_string = lift String $ between (char '"') (char '"') (many $ noneOf ['"'])
ast_func   = lift2 Func (sepBy1 name spaces1) (spaces >> char '=' >> (notFollowedBy $ char '=') >> spaces >> top)
ast_if     = lift3 If (string "if" >> spaces >> top) (spaces >> top) (spaces >> top)
ast_apply  = with_args
         <|> without_args
    where
        --with_args = inner '(' ')' (lift2 Apply (sepBy1 name (char '.')) (many $ spaces >> top))
        with_args = lift2 Apply (sepBy1 name (char '.')) (many $ spaces >> top)
        without_args = lift2 Apply (sepBy1 name (char '.')) (return [])

-- container
ast_list   = lift List $ inner '[' ']' (sepBy top separator)
ast_struct = lift Struct $ inner '{' '}' (sepBy define separator)

-- expression
operator = do
    spaces
    l <- factor
    option l $ op2 l
  where
    op2 l = do
        spaces
        op <- ops
        spaces
        r <- expression
        return $ Op op l r

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
define    = do
    key <- name
    val <- (algebric key) <|> top
    return (key, val)
  where
    algebric key = do
      args <- many (spaces1 >> name)
      spaces
      lookAhead $ oneOf "|}\r\n"
      return $ Tag key args []
inner l r p = between (rtrim $ char l) (ltrim $ char r) p
  where
    ltrim f = white_space >> f
    rtrim f = f >> white_space
    white_space = many $ oneOf " \r\n\t"
separator = (spaces >> (oneOf ",|") >> spaces)
        <|> (many1 $ oneOf " \r\n\t")

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
