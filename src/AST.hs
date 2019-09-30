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
data Define = Define { ssa :: Int, body :: [String] } deriving (Show)
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

nssa d = 1 + ssa d
emit x = Compiler $ \d -> (ssa d, d { body = (' ' : ' ' : x) : (body d) })
next x = Compiler $ \d -> (nssa d, d { body = ("  %" ++ (show $ nssa d) ++ " = " ++ x) : (body d), ssa = nssa d })
alloca ty = next $ "alloca i" ++ show ty ++ ", align 4"
store ty n v = emit $ "store i" ++ show ty ++ " " ++ v ++ ", i" ++ show ty ++ "* " ++ "%" ++ show n ++ ", align 4"
assign ty v = alloca ty >>= \n -> store ty n v
add ty op1 op2 = next $ "add i" ++ show ty ++ " " ++ show op1 ++ ", " ++ show op2
compileToLL name ty f = let
  (_, d) = runCompile f (Define 0 [])
  load = "  %" ++ (show $ nssa d) ++ "= load i32, i32* %" ++ (show $ ssa d) ++ ", align 4\n"
  ret = "  ret i32 %" ++ (show $ nssa d) ++ "\n"
  in "define i" ++ show ty ++ "  @" ++ name ++ "() #0 {\n" ++ (unlines (reverse $ body d)) ++ load ++ ret ++ "}\n"

-- utility
to_string Void = "_"
to_string (Int x) = show x
to_string (Call name []) = name
to_string (Call name argv) = name ++ "(" ++ (string_join " " $ map to_string argv) ++ ")"

string_join glue [] = ""
string_join glue [x] = x
string_join glue (x:xs) = x ++ glue ++ (string_join glue xs)

debug x = trace ("DEBUG: " ++ show x) (return x)
