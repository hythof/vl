module AST where

import Debug.Trace (trace)
import Control.Applicative

type Env = [(String, AST)]

data AST =
-- simple value
    Void
  | I64 Int
  | Bool Bool
  | String String
  | Def String [String] [AST]
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
data Define = Define {
  register_counter :: Int,
  label_counter :: Int,
  strings :: [String],
  env :: [(String, Register)],
  body :: [String],
  subs :: [String]
  } deriving (Show)
data Compiler a = Compiler { runCompile :: Define -> (a, Define) }

data Register = Register { ast :: AST, rty :: String, reg :: String, mem :: String } deriving (Show, Eq)

rcopy r reg = Register (ast r) (rty r) reg ""

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

n0 d = register_counter d
n1 d = 1 + register_counter d
l1 d = 1 + label_counter d
c0 d = show $ n0 d
c1 d = show $ n1 d
last_register = Compiler $ \d -> ('%' : (show $ n0 d), d)
emit x = Compiler $ \d -> ('%' : (show $ n0 d), d { body = (' ' : ' ' : x) : (body d) })
next x = Compiler $ \d -> ('%' : (show $ n1 d), d { body = ("  %" ++ c1 d ++ " = " ++ x) : (body d), register_counter = n1 d })
inc_register = Compiler $ \d -> (n1 d, d { register_counter = n1 d })
inc_label = Compiler $ \d -> (l1 d, d { label_counter = l1 d })
inc_string s = Compiler $ \d -> ("@.v.ref_str." ++ show (length $ strings d), d {
  strings = def_string d : ref_string d : (strings d) })
    where
      c d = show (length $ strings d)
      l = show $ 1 + length s
      def_name d = "@.v.str." ++ c d
      ref_name d = "@.v.ref_str." ++ c d
      def_string d = def_name d ++ " = private unnamed_addr constant [" ++ l ++ " x i8] c\"" ++ s ++ "\00\", align 1"
      ref_string d = ref_name d ++ " = local_unnamed_addr global i8* getelementptr inbounds ([" ++ l ++ " x i8], [" ++ l ++ " x i8]* " ++ def_name d ++ ", i64 0, i64 0), align 8"
get_strings = Compiler $ \d -> (strings d, d)
register name r = Compiler $ \d -> (r, d { env = (name, r) : env d })
reference name = Compiler $ \d -> (ref name $ env d, d)
  where
    ref name xs = case lookup name xs of
      Just x -> x
      Nothing -> error $ "Not found " ++ name
define_sub :: String -> Compiler ()
define_sub body = Compiler $ \d -> ((), d { subs = body : subs d })

-- utility
to_string Void = "_"
to_string (I64 x) = show x
to_string (Bool True) = "true"
to_string (Bool False) = "false"
to_string (String s) = s
to_string (Call name []) = name
to_string (Call name argv) = name ++ "(" ++ (string_join " " $ map to_string argv) ++ ")"

string_join glue [] = ""
string_join glue [x] = x
string_join glue (x:xs) = x ++ glue ++ (string_join glue xs)

debug x = trace ("DEBUG: " ++ show x) (return x)
