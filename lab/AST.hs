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
  | Func [String] AST
  | Closure [String] Env AST
  | List [AST]
  | Throw Env
  | Class Env
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
  , klass :: Env
  , block :: Env
  , local :: Env
  , stack :: [(String, Scope)]
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
  vm >>= f = Runtime $ \s -> if (length $ stack s) > 20
    then error $ "Stack over flow\n" ++ dump s
    else case runState vm s of
      Just (v, s') -> runState (f v) s'

modify f = Runtime $ \s -> return ((), f s)
put s = Runtime $ \_ -> return ((), s)
get = Runtime $ \s -> return (s, s)
evalRuntime vm s = case runState vm s of
  Just (v, _) -> trace (show v) v
  Nothing -> error $ "thrown"

-- utility

dump :: Scope -> String
dump s = "Local:" ++ (kvs $ filter ignore $ local s) ++
    "\nBlock:" ++ (kvs $ filter ignore $ block s) ++
    "\nKlass:" ++ (kvs $ filter ignore $ klass s) ++
    "\nStacks:\n"  ++ showStacks (stack s) ++
    "\nGlobal:" ++ (kvs $ filter ignore $ klass s)
  where
    ignore (_, Func _ _) = False
    ignore (_, Block _) = False
    ignore (_, Throw _) = False
    ignore (_, Call _ _) = False
    ignore _ = True

string_join :: String -> [String] -> String
string_join glue [] = ""
string_join glue [x] = x
string_join glue (x:xs) = x ++ glue ++ (string_join glue xs)

showStacks xs = string_join "\n" $ map showStack xs
showStack (name, s)  = "# " ++ name ++ (kvs $ block s ++ local s)

keys env = (string_join ", " $ map fst env)
kvs [] = ""
kvs env = "\n- " ++ (string_join "\n- " $ map (\(k,v) -> k ++ " " ++ (take 120 $ show v)) env)

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
to_string (Func _ _) = "(func)"
to_string (List xs) = "[" ++ (string_join ", " (map to_string xs)) ++ "]"
to_string (Class xs) = "(class)"
to_string (Match conds) = "(match" ++ (show $ length conds) ++ ")"
