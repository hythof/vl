module Parser where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Expr as Expr
import Control.Applicative ((<*>), (*>), (<*), (<$>), (<$))
import Data.Char (digitToInt)
import AST
import Debug.Trace(trace)

-- exports
parse_program :: String -> [(String, AST)]
parse_program code =
  case parse parse_decls "" code of
    Left err -> [("ERROR", AST.Error (show err))]
    Right ok -> ok

data LineResult =
    LineError String
  | LineDefine (String, AST)
  | LineExp AST
  deriving Show
parse_line :: String -> LineResult
parse_line code = 
  case parse (parse_any) "" code of
    Left err -> LineError $ show err
    Right res -> res
  where
    parse_any = do
        spaces
        ok <- try func <|> exp
        spaces
        rest <- many $ noneOf []
        case rest of
          "" -> return $ ok
          _ -> return $ LineError $ "ParseError : " ++ rest ++ " ok=" ++ (show ok)
    func = LineDefine <$> parse_func
    exp = do
        e <- parse_exp
        return $ LineExp e

---------- parse AST
-- decl
parse_decls = do
    xs <- many parse_decl
    eof
    return xs

parse_decl = try parse_func
         <|> parse_const -- alias from(n 0) to (n = 0)
         <?> "miss decl"

parse_func = do
    rightSpaces
    name <- identifier
    args <- sepBy identifier spaces
    char '='
    rightSpaces
    ret <- parse_exp
    spaces
    let v = if length args == 0 then ret else Func args ret
    return $ (name, v)

parse_const = do
    rightSpaces
    name <- identifier
    rightSpaces
    value <- parse_exp
    spaces
    return $ (name, value)

-- expr or value
parse_exp = Expr.buildExpressionParser table parse_factor
  where
    table = [
        [
          op "*" Expr.AssocLeft,
          op "/" Expr.AssocLeft,
          op "%" Expr.AssocLeft,
          op "//" Expr.AssocLeft,
          op "<<" Expr.AssocLeft,
          op ">>" Expr.AssocLeft,
          op "==" Expr.AssocLeft,
          op "**" Expr.AssocRight -- like math : 2 ^ (3 ^ 4)
        ],
        [
          op "+" Expr.AssocLeft,
          op "-" Expr.AssocLeft,
          op "|" Expr.AssocLeft,
          op "&" Expr.AssocLeft
        ]
      ]
    op name assoc = Expr.Infix (do{reservedOp name; return (Op name)}) assoc

parse_factor = try (parens parse_exp)
           <|> try parse_if
           <|> try parse_switch
           <|> try parse_lookup_func
           <|> try parse_value
           <?> "miss factor"

parse_value = try parse_float
          <|> try (parens parse_integer)
          <|> try parse_natural
          <|> try parse_bool
          <|> try parse_list
          <|> try parse_struct
          <|> try parse_string
          <|> try (parens parse_lookup_func)
          <|> try parse_algebra
          <|> try parse_lookup_const
          <?> "miss value"

parse_float = Real <$> float

parse_integer = Int <$> integer

parse_natural = Int <$> natural

parse_bool = do
    token <- oneOf "TF"
    rightSpaces
    return $ Bool $ token == 'T'

parse_string = do
    v <- between (char '"') (char '"') $ many $ noneOf ['"', '\\']
    rightSpaces
    return $ String v

parse_list = List <$> brackets (sepBy parse_factor spaces)

parse_struct = Struct <$> braces (sepBy parse_decl spaces)

parse_lookup_func = do
    obj <- sepBy1 (many1 $ oneOf "abcdefghijklmnopqrstuvxwyz") (char '.')
    rightSpaces
    args <- sepBy arg (lookAhead $ noneOf "\r\n")
    rightSpaces
    return $ Lookup obj args
  where
    arg = try (parens parse_exp)
      <|> try parse_value
      <?> "miss factor"

parse_lookup_const = do
    obj <- sepBy1 (many1 $ oneOf "abcdefghijklmnopqrstuvxwyz") (char '.')
    rightSpaces
    return $ Lookup obj []

parse_algebra = do
    x <- many1 $ typeName
    lexeme $ string "| "
    xs <- sepBy1 (many1 $ typeName) (lexeme $ char '|')
    return $ Algebra $ x:xs

parse_if = do
    string "if"
    rightSpaces
    cond <- parse_exp
    rightSpaces
    string "then"
    rightSpaces
    t <- parse_exp
    string "else"
    rightSpaces
    f <- parse_exp
    -- return $ If cond t f
    return $ Case [(cond, t), (Bool True, f)]

parse_switch = do
    string "switch"
    rightSpaces
    base <- parse_exp
    rightSpaces
    conds <- many1 (conditions base)
    return $ Case conds
  where
    conditions base = try switch_all <|> try (switch_eq base)
    switch_all = do
        string "\n"
        rightSpaces
        string "_"
        rightSpaces
        char ':'
        rightSpaces
        r <- parse_exp
        rightSpaces
        return (Bool True, r)
    switch_eq base = do
        string "\n"
        rightSpaces
        l <- parse_exp
        rightSpaces
        char ':'
        rightSpaces
        r <- parse_exp
        rightSpaces
        return (Op "==" base l, r)

-- define misc

rightSpaces = skipMany $ oneOf " \t"

lexer :: P.TokenParser ()
lexer      = P.makeTokenParser Lang.emptyDef
reservedOp = P.reservedOp lexer
identifier = do 
    x <- oneOf "abcdefghijklmnopqrstuvxwyz"
    xs <- many $ oneOf "abcdefghijklmnopqrstuvxwyzABCDEFGHIJKLMNOPQRSTUVXWYZ0123456789_"
    rightSpaces
    return $ x:xs
typeName = do
    x <- oneOf "ABCDEFGHIJKLMNOPQRSTUVXWYZ"
    xs <- many $ oneOf "abcdefghijklmnopqrstuvxwyzABCDEFGHIJKLMNOPQRSTUVXWYZ0123456789_"
    rightSpaces
    return $ x:xs
quotes p   = between (lexeme $ char '"') (lexeme $ char '"') p
parens p   = between (lexeme $ char '(') (lexeme $ char ')') p
braces p   = between (lexeme $ char '{') (lexeme $ char '}') p
angles p   = between (lexeme $ char '<') (lexeme $ char '>') p
brackets p = between (lexeme $ char '[') (lexeme $ char ']') p

-- custom lexeme
lexeme p = do
    v <- p
    rightSpaces
    return v

-- copy from Text.Parsec.Token
-----------------------------------------------------------
-- Numbers
-----------------------------------------------------------
naturalOrFloat  = lexeme (natFloat) <?> "number"

float           = lexeme floating   <?> "float"
integer         = lexeme int        <?> "integer"
natural         = lexeme nat        <?> "natural"

-- floats
floating        = do{ n <- decimal
                    ; fractExponent n
                    }


natFloat        = do{ char '0'
                    ; zeroNumFloat
                    }
                  <|> decimalFloat

zeroNumFloat    =  do{ n <- hexadecimal <|> octal
                     ; return (Left n)
                     }
                <|> decimalFloat
                <|> fractFloat 0
                <|> return (Left 0)

decimalFloat    = do{ n <- decimal
                    ; option (Left n)
                             (fractFloat n)
                    }

fractFloat n    = do{ f <- fractExponent n
                    ; return (Right f)
                    }

fractExponent n = do{ fract <- fraction
                    ; expo  <- option 1.0 exponent'
                    ; return ((fromInteger n + fract)*expo)
                    }
                <|>
                  do{ expo <- exponent'
                    ; return ((fromInteger n)*expo)
                    }

fraction        = do{ char '.'
                    ; digits <- many1 digit <?> "fraction"
                    ; return (foldr op 0.0 digits)
                    }
                  <?> "fraction"
                where
                  op d f    = (f + fromIntegral (digitToInt d))/10.0

exponent'       = do{ oneOf "eE"
                    ; f <- sign
                    ; e <- decimal <?> "exponent"
                    ; return (power (f e))
                    }
                  <?> "exponent"
                where
                   power e  | e < 0      = 1.0/power(-e)
                            | otherwise  = fromInteger (10^e)


-- integers and naturals
int             = do{ f <- lexeme sign
                    ; n <- nat
                    ; return (f n)
                    }

sign            =   (char '-' >> return negate)
                <|> (char '+' >> return id)
                <|> return id

nat             = zeroNumber <|> decimal

zeroNumber      = do{ char '0'
                    ; hexadecimal <|> octal <|> decimal <|> return 0
                    }
                  <?> ""

decimal         = number 10 digit
hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
octal           = do{ oneOf "oO"; number 8 octDigit  }

number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }
