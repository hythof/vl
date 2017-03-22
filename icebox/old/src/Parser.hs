module Parser where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Expr as Expr
import Control.Applicative ((<*>), (*>), (<*), (<$>), (<$))
import AST

-- define misc
languageDef :: Lang.LanguageDef st
languageDef = Lang.haskellDef 
lexer :: P.TokenParser ()
lexer       = P.makeTokenParser languageDef
identifier  = P.identifier lexer
reserved    = P.reserved lexer
operator    = P.operator lexer
reservedOp  = P.reservedOp lexer
natural     = P.natural lexer
integer     = P.integer lexer
float       = P.float lexer
symbol      = P.symbol lexer
lexeme      = P.lexeme lexer
whiteSpace  = P.whiteSpace lexer
parens      = P.parens lexer
braces      = P.braces lexer
angles      = P.angles lexer
brackets    = P.brackets lexer
semi        = P.semi lexer
comma       = P.comma lexer
colon       = P.colon lexer
dot         = P.dot lexer
semiSep     = P.semiSep lexer
semiSep1    = P.semiSep1 lexer
commaSep    = P.commaSep lexer
commaSep1   = P.commaSep1 lexer
pneg::Parser String
pneg        = option "" (string "-")

-- tiny test
view p input =
  case (parse p "" input) of
    Left err -> print err
    Right x -> print x

-- entry point
parse_program :: String -> AST
parse_program code =
  case parse (sepBy parse_decl spaces) "" code of
    Left err -> DeclVar "error" $ String $ show err
    Right res -> Struct res

-- decl or statement
parse_decl = try parse_func
         <|> try parse_var
--  <|> try decl_stmt

parse_func = do
  name <- identifier
  whiteSpace
  args <- option [] (sepBy identifier whiteSpace)
  string "="
  whiteSpace
  ret <- parse_exp
  return $ DeclFunc name args ret

-- bind
parse_var = do
  name <- identifier
  value <- parse_value
  return $ DeclVar name value

-- expr or value
parse_exp = try parse_op
            <|> parse_value

parse_value = try parse_float
          <|> try parse_integer
          <|> try parse_string
          <|> try parse_bool
          <|> try parse_list
          <|> try parse_struct
          <|> try parse_lookup

parse_op = Expr.buildExpressionParser table parse_value
  where
    table = [
      [op "*" Expr.AssocLeft, op "/" Expr.AssocLeft],
      [op "+" Expr.AssocLeft, op "-" Expr.AssocLeft]
      ]
    op name assoc = Expr.Infix (do{ reservedOp name; return (Op name)}) assoc

parse_float = Double <$> float

parse_integer = Int <$> integer

parse_bool = Bool <$> oneOf "TF"

parse_string = String <$> v
  where
    v  = between dq dq $ many $ noneOf ['"', '\\']
    dq = (char '"')

parse_list = List <$> v
  where
    v    = pair $ sepBy parse_exp spaces
    pair = between (symbol "[") (symbol "]")

parse_struct = Struct <$> v
  where
    v    = pair $ sepBy parse_decl spaces
    pair = between (symbol "{") (symbol "}")

parse_lookup = do
    obj <- sepBy1 identifier (string ".")
    whiteSpace
    args <- sepBy parse_exp whiteSpace
    return $ Lookup obj args
  
eol :: Parser String
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <|> string ";"
  <?> fail "Couldn't find EOL"
