module Parse where

import Define

import Debug.Trace (trace)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P
import Control.Applicative ((<*>), (*>), (<*), (<$>), (<$))

-- exports
program :: String -> String -> Either ParseError [(String, PT)]
program code name = parse top name code

inference :: PT -> AST
inference ast = Byte 0

optimize :: AST -> AST
optimize ast = ast

-- private
top = do
    xs <- defines
    eof
    return xs

defines = sepBy define spaces

define = try defineFunc
     <|> try defineValue
     <?> "define"

defineFunc = do
    name <- identifier
    v <- func
    return $ (name, v)

defineValue = do
    name <- identifier
    v <- value
    return $ (name, v)

expression = try pif
    <|> try op
    <|> try value
    <|> try apply
    <|> try (pair '(' ')' expression)
    <?> "expression"

value = try (pair '(' ')' func)
    <|> try array
    <|> try struct
    <|> try (PTyped <$> primitive)
    <?> "value"

primitive = try byte
    <|> try float
    <|> try integer
    <|> try bool
    <|> try text
    <|> try rune
    <?> "value"

pif = do
    lexeme $ string "if"
    v1 <- lexeme expression
    v2 <- lexeme expression
    v3 <- lexeme expression
    return $ PIf v1 v2 v3

op = do
    input <- getInput
    s <- source
    v1 <- value
    op <- lexeme1 $ oneOf "+-/*<=>|&%"
    v2 <- value
    return $ PApply op [v1, v2] s

apply = do
    s <- source
    name <- identifier
    current <- indent
    xs <- sepBy expression (skip (1 + current))
    return $ PApply name xs s
  where
    skip n = try $ next n
         <|> many blank
    next n = do
        many $ blank
        char '\n'
        string (take n $ repeat ' ')

func = do
   args <- many identifier
   lexeme $ char '='
   v <- lexeme $ expression
   return $ PFunc args v

array = PArray <$> pair '[' ']' (many expression)

struct = PStruct <$> pair '{' '}' defines

byte = do
    string "0x"
    l <- oneOf "0123456789abcdef"
    r <- oneOf "0123456789abcdef"
    let v = ((num l) * 16 + (num r))
    return $ Byte v
  where
    num n = case n of
      '0' -> 0
      '1' -> 1
      '2' -> 2
      '3' -> 3
      '4' -> 4
      '5' -> 5
      '6' -> 6
      '7' -> 7
      '8' -> 8
      '9' -> 9
      'a' -> 10
      'b' -> 11
      'c' -> 12
      'd' -> 13
      'e' -> 14
      'f' -> 15

float = do
    v1 <- lexeme1 digit
    char '.'
    v2 <- lexeme1 digit
    let v = v1 ++ "." ++ v2
    return $ Float (read v)

integer = Int <$> read <$> lexeme1 digit

bool = Bool <$> (== 'T') <$> oneOf "TF"

text = String <$> pair '"' '"' (lexeme0 $ noneOf "\"")

rune = Char <$> (pair '\'' '\'' $ noneOf "\'")

-- utility
blank = oneOf " \t"

lexeme p = do
    v <- p
    many $ blank
    return v
lexeme0 p = lexeme $ many p
lexeme1 p = lexeme $ many1 p

identifier = lexeme $ many1 letter

pair l r = between (char l) (char r)

source = do
    s <- getPosition
    let name = sourceName s
    let line = sourceLine s
    let column = sourceColumn s
    return $ Source name line column

indent = do
    input <- getInput
    pos <- getPosition
    let index = (sourceLine pos) - 1
    return $ countIndent (lines input !! index)

countIndent (' ':xs) = 1 + countIndent xs
countIndent _ = 0
