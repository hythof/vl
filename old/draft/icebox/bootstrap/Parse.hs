module Parse (parse) where

import Define

import Debug.Trace (trace)
import Text.Parsec hiding (parse)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P
import Control.Applicative ((<*>), (*>), (<*), (<$>), (<$))

-- exports
parse :: String -> Either ParseError [(String, AST)]
parse src = runP top () "" src
  where
    top = do
        xs <- defines
        eof
        return xs

    indent = do
        pos <- getPosition
        let index = (sourceLine pos) - 1
        return $ countIndent (lines src !! index)

    defines = many define
    
    define = do
        name <- lexeme $ identifier
        v <- if any (== head name) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
          then type_ name
          else try func<|> value
        spaces
        return (name, v)
    
    func = do
       args <- many identifier
       lexeme $ char '='
       v <- expression
       many blank
       return $ Func args v

    type_ name = do
        current <- indent
        char '|'
        lexeme $ char '\n'
        xs <- sepBy (many1 identifier) (awayIndent (1 + current))
        return $ Class name xs
    
    value = try (pair '(' ')' func)
        <|> try array
        <|> try struct
        <|> try text
        <|> try rune
        <|> try byte
        <|> try bool
        <|> try float
        <|> try integer
        <?> "value"
    
    expression = try (pair '(' ')' (try apply <|> expression))
             <|> try switch
             <|> try op2
             <|> try value
             <|> try apply
             <?> "expression"
      where
        op2 = p1
        p1 = binary p2 [string "<", string "<=", string  "==", string ">=", string ">", string "&&", string "||"]
        p2 = binary p3 [string "+", string "-", string "|", string "&"]
        p3 = binary p9 [string "*", string "/", string "**"]
        p9 = try value
         <|> try apply
         <|> try (pair '(' ')' (try apply <|> expression))
         <?> "op2 bottom"
        binary f cs = try (binaryOp f cs)
                  <|> try (lexeme f)
                  <?> "binary"
        binaryOp f cs = do
            v1 <- lexeme f
            op <- lexeme (choice cs)
            v2 <- lexeme f
            return $ Op2 op v1 v2

    arg = try (pair '(' ')' (try apply <|> expression))
       <|> try value
       <|> try ref
       <?> "arg"
    
    switch = try case_ <|> try if_ <?> "switch"

    case_ = do
        lexeme $ string "case"
        v1 <- lexeme arg
        lexeme $ char '\n'
        v2 <- lexeme $ many1 (try pattern <|> try condition)
        v3 <- lexeme expression
        return $ Case v1 v2 v3
      where
        pattern = do
            lookAhead $ oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            name <- identifier
            args <- many $ lexeme identifier
            lexeme $ char ':'
            ret <- lexeme expression
            lexeme $ char '\n'
            return ((Match name args), ret)

        condition = do
            cond <- lexeme expression
            lexeme $ char ':'
            ret <- lexeme expression
            lexeme $ char '\n'
            return (cond, ret)

    if_ = do
        lexeme $ string "if"
        v1 <- lexeme arg
        v2 <- lexeme arg
        v3 <- lexeme arg
        return $ If v1 v2 v3

    ref = Ref <$> lexeme identifier

    apply = do
        name <- identifier
        current <- indent
        xs <- sepBy arg (awayIndent (1 + current))
        return $ if length xs == 0 then Ref name else Apply name xs

    array = Array <$> pair '[' ']' (many expression)
    
    struct = Struct <$> pair '{' '}' defines
    
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
        v1 <- lexeme $ many1 digit
        char '.'
        v2 <- lexeme $ many1 digit
        let v = v1 ++ "." ++ v2
        return $ Float (read v)
    
    integer = Int <$> read <$> lexeme (many1 digit)
    
    --bool = Bool <$> (== 'T') <$> lexeme (oneOf "TF")
    bool = do
        c <- lexeme (oneOf "TF")
        return $ Bool ('T' == c)
    
    text = String <$> pair '"' '"' (lexeme (many $ noneOf "\""))
    
    rune = Char <$> (pair '\'' '\'' $ noneOf "\'")

    debug s = trace ("\n  " ++ show s ++ " # " ++ src) (many $ string "@@@@@@@@@@@")
    debugIf c s = if c then trace ("\n  " ++ show s ++ " # " ++ src) (return ()) else return ()
    
-- utility
blank = oneOf " \t"

lexeme p = do
    v <- p
    many $ blank
    return v

identifier = do
    c <- letter
    cs <- lexeme (many (alphaNum <|> oneOf "_."))
    return $ c : cs

pair l r = between (char l) (char r)

awayIndent n = try $ next n
     <|> many blank
  where
    next n = do
        many blank
        char '\n'
        string (take n $ repeat ' ')

countIndent (' ':xs) = 1 + countIndent xs
countIndent _ = 0
