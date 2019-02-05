module ParserLib where

import           AST
import           Control.Applicative (Alternative, Applicative, empty, pure,
                                      (*>), (<*>), (<|>))
import           Debug.Trace         (trace)

data Source = Source String Int deriving Show
data Parser a = Parser { runParser :: Source -> Maybe (Source, a) }

instance Functor Parser where
    fmap f p = Parser $ \s -> fmap conv $ runParser p s
      where
        conv (s, v) = (s, f v)

instance Applicative Parser where
    pure v = Parser $ \s -> Just (s, v)
    l <*> r = Parser $ \s -> case runParser r s of
        Just (s', v) -> fmap (\(s'', f) -> (s'', f v)) $ runParser l s
        Nothing -> Nothing
    l *> r = Parser $ \s -> case runParser l s of
        Just (s', v) -> runParser r s'
        Nothing -> Nothing

instance Monad Parser where
    return = pure
    m >>= f = Parser $ \s -> case runParser m s of
        Just (s', v) -> runParser (f v) s'
        Nothing -> Nothing
    (>>) = (*>)
    fail _ = Parser $ \_ -> Nothing

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    l <|> r = Parser $ \s -> runParser l s <|> runParser r s

consume :: (Char -> Bool) -> Parser Char
consume f = Parser $ \(Source s p) -> if p < length s && f (s !! p)
    then Just (Source s $ p + 1, s !! p)
    else Nothing

eol :: Parser ()
eol = (lookAhead $ char '\n' >> return ()) <|> eof

eof :: Parser ()
eof = Parser $ \(Source s p) -> if p == length s
  then Just (Source s p, ())
  else Nothing

rest :: Parser String
rest = Parser $ \(Source s p) -> Just ((Source s (length s)), take (length s - p) (drop p s))

lookAhead :: Parser a -> Parser a
lookAhead p = Parser $ \s -> fmap (\(_, v) -> (s, v)) $ runParser p s

notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = Parser $ \s -> case runParser p s of
    Just _ -> Nothing
    Nothing -> Just (s, ())

many :: Parser a -> Parser [a]
many p = Parser $ \s -> recursion s []
  where
    recursion s acc = case runParser p s of
        Just (s', x) -> recursion s' (x:acc)
        Nothing -> Just (s, reverse acc)

many1 :: Parser a -> Parser [a]
many1 p = do
    x <- p
    xs <- many p
    return $ x:xs

char :: Char -> Parser Char
char x = consume (== x)

oneOf :: [Char] -> Parser Char
oneOf xs = consume (\c -> elem c xs)

noneOf :: [Char] -> Parser Char
noneOf xs = consume (\c -> not $ elem c xs)

string :: String -> Parser String
string s = match s []
  where
    match [] acc = return $ reverse acc
    match (x:xs) acc = do
        c <- consume (==x)
        match xs (c:acc)

between :: Parser a -> Parser b -> Parser c -> Parser c
between l r p = do
    l
    v <- p
    r
    return v

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p s = sepBy1 p s <|> return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p s = do
    x <- p
    xs <- many $ do
        s
        y <- p
        return y
    return $ x:xs

option :: a -> Parser a -> Parser a
option v p = Parser $ \s -> runParser p s <|> Just (s, v)

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  many $ oneOf " \t"
  return x

debug :: String -> Parser ()
debug mark = Parser $ \s -> Just (strace s, ())
  where
    strace s@(Source src pos) = trace ("#" ++ mark ++ "   " ++ show s ++ "   " ++ rest pos src) s
    rest _ [] = ""
    rest 0 xs = xs
    rest n (x:xs) = rest (n - 1) xs
