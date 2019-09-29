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


-- LLVM
data LLVM = LLVM { strs :: [String], defines :: [Define] } deriving (Show)

data Define = Define { body :: [String] } deriving (Show)
data Compiler a = Compiler { runCompile :: Define -> (a, Define) }

instance Functor Compiler where
  fmap f c = Compiler $ \d ->let (a, d') = (runCompile c d) in (f a, d')

instance Applicative Compiler where
  pure v = Compiler $ \d -> (v, d)
  l <*> r = Compiler $ \d ->
    let
      (f, d') = runCompile l d
      (a, d'') = runCompile r d'
    in (f a, d'')

instance Monad Compiler where
  return = pure
  l >>= f = Compiler $ \d -> let (a, d') = runCompile l d in runCompile (f a) d'

emmit x = Compiler $ \d -> ((), d { body = (' ' : ' ' : x) : (body d) })
compileToLL name ret f = let
  (_, d) = runCompile f (Define [])
  in "define " ++ ret  ++ "  @" ++ name ++ "() #0 {\n" ++ (unlines (reverse $ body d)) ++ "}\n"

-- utility
to_string Void = "_"
to_string (Int x) = show x
to_string (Call name []) = name
to_string (Call name argv) = name ++ "(" ++ (string_join " " $ map to_string argv) ++ ")"

string_join glue [] = ""
string_join glue [x] = x
string_join glue (x:xs) = x ++ glue ++ (string_join glue xs)

debug x = trace ("DEBUG: " ++ show x) (return x)
