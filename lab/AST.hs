module AST where

import Debug.Trace (trace)

type Env = [(String, AST)]

data AST =
-- simple value
    Void
  | Bool Bool
  | Int Int
  | Float Double
  | String String
  | Func [String] Env AST
  | List [AST]
  | Throw String
  | Struct String Env
  | New String [String] Env
-- expression
  | Block [AST]
  | Assign String AST
  | Update String AST
  | Call String [AST] -- ref, exp, assign and update
  | Match [([Matcher], AST)]
  deriving (Show, Eq)

data Matcher =
  MatchAny
  | MatchValue AST
  | MatchType String
  deriving (Show, Eq)

-- Pparser
data Source = Source { original :: String, source :: String, indentation :: Int, line :: Int, column :: Int } deriving (Show)

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



-- Runtime

data Scope = Scope {
    global :: Env -- readonly
  , methods :: Env -- in context as struct, updated values
  --, context :: Env -- arguments, assigned values
  , vars :: Env -- deplicated
  , block :: Env -- deplicated
  , local :: Env -- deplicated
  , stack :: [(String, Scope)]        -- for debug
  , history :: [(String, [AST], AST)] -- for debug
  , throws :: [(String)]              -- for debug
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
  vm >>= f = Runtime $ \s -> if (length $ stack s) > 100
    then error $ "Stack over flow\n" ++ dump s
    else case runState vm s of
      Right (v, s') -> runState (f v) s'
      Left (e, s') -> Left (e, s')

modify f = Runtime $ \s -> return ((), f s)
put s = Runtime $ \_ -> return ((), s)
get = Runtime $ \s -> return (s, s)
evalRuntime vm s = case runState vm s of
  Right (v, _) -> v
  Left (e, s) -> error $ show e ++ "\n" ++ dump s

-- utility

dump :: Scope -> String
dump s = "Local:" ++ (kvs $ local s)
    ++ "\n\nBlock:" ++ (kvs $ block s)
    ++ "\n\nVars:" ++ (kvs $ vars s)
    ++ "\n\nMethods:" ++ (kvs $ methods s)
    ++ "\n\nStacks:\n"  ++ showStacks (take 5 $ stack s)
    ++ "\n\nHistory:\n" ++ showHistories (history s)
    ++ "\n\nThrows:\n" ++ showThrows (take 5 $ throws s)
    ++ "\n\nGlobal:\n" ++ (kvs $ global s)

string_join :: String -> [String] -> String
string_join glue [] = ""
string_join glue [x] = x
string_join glue (x:xs) = x ++ glue ++ (string_join glue xs)

showStacks xs = string_join "\n" $ map showStack xs
showStack (name, s)  = "# " ++ name ++ (kvs $ vars s ++ block s ++ local s)
showHistories xs = string_join "\n" $ map showHistory xs
showHistory (name, argv, ret)  = "# " ++ name ++ (if length argv == 0 then "" else "(" ++ (string_join "," $ map to_string argv) ++ ")") ++ "\t-> " ++ (to_string ret)
showThrows xs = string_join "\n" $ map showThrow xs
showThrow x = "# " ++ x

keys env = (string_join ", " $ map fst env)
kvs [] = ""
kvs env = "\n- " ++ (string_join "\n- " $ map (\(k,v) -> k ++ " " ++ (take 120 $ to_string v)) env)

string_contains :: String -> String -> Bool
string_contains target y = go target
  where
    n = length y
    go [] = False
    go str@(x:xs) = (take n str == y) || go xs

string_replace :: Int -> String -> String -> String -> String
string_replace n a b c = string_join c (string_split n a b)
string_split :: Int -> String -> String -> [String]
string_split limit str delim = go limit str [] []
  where
    n = length delim
    go 0 str _ acc2 = acc2 ++ [str]
    go m [] _ acc2 = acc2
    go m str@(x:xs) acc1 acc2 = if delim == (take n str)
      then go (m - 1) (drop n str) [] (if length acc1 == 0 then acc2 else (reverse acc1) : acc2)
      else go m xs (x : acc1) acc2

to_strings xs = string_join " " (map show xs)
to_string Void = "_"
to_string (Bool True) = "true"
to_string (Bool False) = "false"
to_string (Int x) = show x
to_string (Float x) = show x
to_string (String "") = "(empty string)"
to_string (String x) = x
to_string (Func args _ body) = "(" ++ (string_join "," args) ++ " => " ++ to_string body ++ ")"
to_string (Call name []) = name
to_string (Call name argv) = name ++ "(" ++ (string_join " " $ map to_string argv) ++ ")"
to_string (Assign name body) = name ++ " = " ++ to_string body
to_string (Update name body) = name ++ " := " ++ to_string body
to_string (Block xs) = string_join "; " (map to_string xs)
to_string (List xs) = "[" ++ (string_join ", " (map to_string xs)) ++ "]"
to_string (Struct name _) = "(struct: " ++ name ++ ")"
to_string (New name _ _) = "(new: " ++ name ++ ")"
to_string (Throw x) = "(throw: " ++ x ++ ")"
to_string (Match conds) = "(match " ++ (show $ length conds) ++ ")"
--to_string x = show x

debug x = trace ("@ " ++ (take 200 $ show x)) (return ())
