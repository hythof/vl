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
  | Throw Env
  | Class Env Env -- properties, methods
-- expression
  | Block [AST]
  | Assign String AST
  | Update String AST
  | Call String [AST] -- ref, exp, assign and update
  | Match [([AST], AST)]
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
    global :: Env
  , methods :: Env
  , vars :: Env
  , block :: Env
  , local :: Env
  , stack :: [(String, Scope)]
  , history :: [(String, [AST], AST)]
  , throws :: [(String)]
} deriving (Show, Eq)

data Runtime a = Runtime { runState :: Scope -> Maybe (a, Scope) }

instance Functor Runtime where
  fmap f vm = Runtime $ \s -> fmap (\(a, s') -> (f a, s')) (runState vm s)

instance Applicative Runtime where
  pure v = Runtime $ \s -> Just (v, s)
  l <*> r = Runtime $ \s ->
            case runState l s of
              Just (f, s') -> case runState r s' of
                Just (v, s'') -> Just (f v, s'')
                _ -> Nothing
              _ -> Nothing

instance Monad Runtime where
  return = pure
  vm >>= f = Runtime $ \s -> if (length $ stack s) > 100
    then error $ "Stack over flow\n" ++ dump s
    else case runState vm s of
      Just (v, s') -> runState (f v) s'
      Nothing -> Nothing

modify f = Runtime $ \s -> return ((), f s)
put s = Runtime $ \_ -> return ((), s)
get = Runtime $ \s -> return (s, s)
evalRuntime vm s = case runState vm s of
  Just (v, _) -> v
  Nothing -> error $ "thrown"

-- utility

dump :: Scope -> String
dump s = "Local:" ++ (kvs $ local s)
    ++ "\nBlock:" ++ (kvs $ block s)
    ++ "\nVars:" ++ (kvs $ vars s)
    ++ "\nStacks:\n"  ++ showStacks (take 3 $ stack s)
    ++ "\nHistory:\n"  ++ showHistories (take 10 $ history s)
    ++ "\nThrows:\n"  ++ showThrows (take 10 $ throws s)
    --"\nGlobal:" ++ (kvs $ global s)

string_join :: String -> [String] -> String
string_join glue [] = ""
string_join glue [x] = x
string_join glue (x:xs) = x ++ glue ++ (string_join glue xs)

showStacks xs = string_join "\n" $ map showStack xs
showStack (name, s)  = "# " ++ name ++ (kvs $ vars s ++ block s ++ local s)
showHistories xs = string_join "\n" $ map showHistory xs
showHistory (name, argv, ret)  = "# " ++ (to_string ret) ++ " = " ++ name ++ "(" ++ (string_join "," $ map to_string argv) ++ ")"
showThrows xs = string_join "\n" $ map showThrow xs
showThrow x = x

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
to_string (String x) = x
to_string (Func args _ body) = "(" ++ (string_join "," args) ++ " => " ++ to_string body ++ ")"
to_string (Call name []) = name
to_string (Call name argv) = name ++ "(" ++ (string_join " " $ map to_string argv) ++ ")"
to_string (Assign name body) = name ++ " = " ++ to_string body
to_string (Update name body) = name ++ " := " ++ to_string body
to_string (Block xs) = string_join "; " (map to_string xs)
to_string (List xs) = "[" ++ (string_join ", " (map to_string xs)) ++ "]"
to_string (Class vars methods) = "(class: " ++ kvs vars ++ ")"
to_string (Throw xs) = "(throw: " ++ (string_join ", " $ map fst xs) ++ ")"
to_string (Match conds) = "(match" ++ (show $ length conds) ++ ")"
--to_string x = show x

debug x = trace ("@ " ++ (take 200 $ show x)) (return ())
