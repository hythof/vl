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

-- utility
parseProgram :: String -> Either ParseError [(String, AST)]
parseProgram code = parse parseDecls "" code

-- decl
parseDecls = do
    xs <- many parseDecl
    eof
    return xs

parseDecl = try parseDeclFunc
             <|> parseDeclConst

parseDeclFunc = do
    rightSpaces
    name <- identifier
    args <- sepBy identifier spaces
    char '='
    rightSpaces
    ret <- parseExp
    spaces
    let v = if length args == 0 then ret else Func args ret
    return $ (name, v)

parseDeclConst = do
    rightSpaces
    name <- identifier
    rightSpaces
    ret <- parseExp
    spaces
    return $ (name, ret)

-- expr or value
parseExp = Expr.buildExpressionParser table parseFactor
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

parseFactor = try (parens parseExp)
           <|> try parseIf
           <|> try parseLookupFunc
           <|> try parseValue
           <?> "miss factor"

parseValue = try parseFloat
          <|> try (parens parseInteger)
          <|> try parseNatural
          <|> try parseBool
          <|> try parseList
          <|> try parseStruct
          <|> try parseString
          <|> try (parens parseLookupFunc)
          <|> try parseLookupConst
          <?> "miss value"

parseFloat = Float <$> float

parseInteger = Int <$> integer

parseNatural = Int <$> natural

parseBool = do
    token <- oneOf "TF"
    rightSpaces
    return $ Bool $ token == 'T'

parseString = do
    v <- between (char '"') (char '"') $ many $ noneOf ['"', '\\']
    rightSpaces
    return $ String v

parseList = List <$> brackets (sepBy parseFactor spaces)

parseStruct = Struct <$> braces (sepBy parseDecl spaces)

parseLookupFunc = do
    obj <- sepBy1 (many1 $ oneOf "abcdefghijklmnopqrstuvxwyz") (char '.')
    rightSpaces
    args <- sepBy arg (lookAhead $ noneOf "\r\n")
    rightSpaces
    return $ Lookup obj args
  where
    arg = try (parens parseExp)
      <|> try parseValue
      <?> "miss factor"

parseLookupConst = do
    obj <- sepBy1 (many1 $ oneOf "abcdefghijklmnopqrstuvxwyz") (char '.')
    rightSpaces
    return $ Lookup obj []

parseIf = do
    string "if"
    rightSpaces
    cond <- parseExp
    rightSpaces
    string "then"
    rightSpaces
    t <- parseExp
    string "else"
    rightSpaces
    f <- parseExp
    return $ If cond t f

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
