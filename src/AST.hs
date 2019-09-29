module AST where

import Debug.Trace (trace)
import Control.Applicative

type Env = [(String, AST)]

data AST =
-- simple value
    Void
  | Int Int
  | Call String [AST]
  deriving (Show, Eq)

-- Pparser
data Source = Source { src :: String, pos :: Int, len :: Int } deriving (Show)

data Parser a = Parser { runParser :: Source -> Maybe (a, Source) }

instance Functor Parser where
  fmap f p = Parser $ \s -> fmap (\(a, ss) -> (f a, ss)) (runParser p s)

instance Applicative Parser where
  pure v = Parser $ \s -> Just (v, s)
  l <*> r = Parser $ \s ->
            case runParser l s of
              Just (f, s') -> case runParser r s' of
                Just (v, s'') -> Just (f v, s'')
                _ -> Nothing
              _ -> Nothing

instance Monad Parser where
  return = pure
  l >>= f = Parser $ \s -> case runParser l s of
    Just (a, ss) -> runParser (f a) ss
    _ -> Nothing

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  l <|> r = Parser $ \s ->
            case runParser l s of
              Just x@(_, _) -> return x
              Nothing -> runParser r s


-- Runtime

data Scope = Scope {
    global :: Env
  , local :: Env
} deriving (Show, Eq)

data Runtime a = Runtime { runState :: Scope -> Either (AST, Scope) (a, Scope) }

instance Functor Runtime where
  fmap f vm = Runtime $ \s -> fmap (\(a, s') -> (f a, s')) (runState vm s)

instance Applicative Runtime where
  pure v = Runtime $ \s -> return (v, s)
  l <*> r = Runtime $ \s ->
            case runState l s of
              Right (f, s') -> case runState r s' of
                Right (v, s'') -> return (f v, s'')
                Left (e, s') -> Left (e, s')
              Left (e, s') -> Left (e, s')

instance Monad Runtime where
  return = pure
  vm >>= f = Runtime $ \s -> case runState vm s of
      Right (v, s') -> runState (f v) s'
      Left (e, s') -> Left (e, s')

modify f = Runtime $ \s -> return ((), f s)
put s = Runtime $ \_ -> return ((), s)
get = Runtime $ \s -> return (s, s)

-- utility
to_string Void = "_"
to_string (Int x) = show x
to_string (Call name []) = name
to_string (Call name argv) = name ++ "(" ++ (string_join " " $ map to_string argv) ++ ")"

string_join glue [] = ""
string_join glue [x] = x
string_join glue (x:xs) = x ++ glue ++ (string_join glue xs)

debug x = trace ("DEBUG: " ++ show x) (return x)
