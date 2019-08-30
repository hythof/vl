module Evaluator where

import Debug.Trace (trace)
import AST
import Parser (parseAST)
import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Foldable (find)

eval :: Env -> AST -> AST
eval env input = evalState (unify input) (Scope env [] [] [])

-- private functions

data Scope = Scope {
    global :: Env
  , share :: Env
  , local :: Env
  , stack :: [(String, Scope)]
} deriving (Show, Eq)

type Runtime = State Scope

unify :: AST -> Runtime AST
unify (List xs) = List <$> mapM unify xs
unify (Block lines) = fmap last $ mapM unify lines
unify (Assign name body) = do
  ret <- unify body
  ret <- unwrap ret
  modify (\s -> s { local = (name, ret) : local s })
  return ret
unify (Call name argv) = do
  argv <- mapM unify argv
  case find is_throw argv of
    Just x -> return x
    Nothing -> callWithStack name argv
unify f@(Func args body) = if elem ("__captured", Void) args then return f else do
  s <- get
  return $ Func (args ++ (local s) ++ [("__captured", Void)]) body
unify ast = return ast

unwrap (Func args body) = do
  s <- get
  put $ s { local = local s ++ args }
  ret <- unify body
  put s
  return ret
unwrap ast = return ast

callWithStack :: String -> [AST] -> Runtime AST
callWithStack name argv = do
  s1 <- get
  put $ s1 { stack = (name ++ " : " ++ (keys $ local s1), s1) : stack s1 }
  ret <- call name argv
  s2 <- get
  put $ s2 { stack = stack s1 }
  return ret
call :: String -> [AST] -> Runtime AST
call "_" [] = return Void
call "|" [l, r] = return $ if is_throw l then l else r
call "||" [(Bool True), _] = return $ Bool True
call "||" [(Bool False), r] = return r
call "&&" [(Bool True), r] = return r
call "&&" [(Bool False), _] = return $ Bool False
call "+" [(Int l), (Int r)] = return $ Int $ l + r
call "-" [(Int l), (Int r)] = return $ Int $ l - r
call "*" [(Int l), (Int r)] = return $ Int $ l * r
call "/" [(Int l), (Int r)] = return $ Int $ div l r
call ">" [(Int l), (Int r)] = return $ Bool (l > r)
call ">=" [(Int l), (Int r)] = return $ Bool (l >= r)
call "<" [(Int l), (Int r)] = return $ Bool (l < r)
call "<=" [(Int l), (Int r)] = return $ Bool (l <= r)
call "." [(String l), (String r)] = return $ String (l ++ r)
call "++" [(List l), (List r)] = return $ List (l ++ r)
call "==" [l, r] = return $ Bool (show l == show r)
call "!=" [l, r] = return $ Bool (not $ (show l == show r))
call "trace" args = trace ("TRACE: " ++ (take 100 $ show args)) (return Void)
call "if"  [Bool a, b, c] = return $ if a then b else c
call "map"  [List xs, ast] = do { x <- mapM (\x -> apply ast [x]) xs; return $ List x }
call "mapi"  [List xs, ast] = List <$> mapM (\(i, arg) -> apply ast [Int i, arg]) (zip [0..] xs)
call "find"  [List xs, ast] = do
  hits <- filterM (\x -> do { y <- apply ast [x]; return (y == Bool True)}) xs
  case hits of
    [] -> miss $ "Finding element not found " ++ show ast ++ "\n  in " ++ show xs
    (x:_) -> return x
call "has"  [List xs, ast] = return $ Bool (elem ast xs)
call "has"  [String xs, String x] = return $ Bool (string_contains xs x)
call "join" [List xs, String glue] = return $ String (string_join glue (map to_string xs))
call "sub"  [String a, String b, String c] = return $ String (string_replace (length a) a b c)
call "rsub1"  [String a, String b, String c] = return $ String (reverse $ string_replace 1 (reverse a) (reverse b) (reverse c))
call "to_int" [String s] = return $ Int (read s :: Int)
call "to_float" [String s] = return $ Float (read s :: Double)
call "to_string" [Int n] = return $ String $ show n
call "to_string" [Float n] = return $ String $ show n
call "to_string" [String s] = return $ String $ '"' :  s ++ "\""
call "not" [Bool b] = return $ Bool $ not b
call "length" [List s] = return $ Int $ length s
call "length" [String s] = return $ Int $ length s
call "slice"  [String s, Int n] = return $ String $ drop n s
call "slice"  [String s, Int n, Int m] = return $ String $ take m (drop n s)
call "at" [String s, Int index] = return $ if index < length s
  then String $ [(s !! index)]
  else throw $ "out of index " ++ show index ++ " in \"" ++ s ++ "\""
call "at" [List s, Int index] = return $ if index < length s
  then s !! index
  else throw $ "out of index " ++ show index ++ " in \"" ++ show s ++ "\""
call name [] = ref name
call name ((Class env):argv) = do
  s1 <- get
  put $ s1 { share = new env argv }
  ret <- call name argv
  s2 <- get
  put $ s2 { share = share s1 }
  return $ ret
call name argv = do
  ast <- ref name
  apply ast argv

apply :: AST -> [AST] -> Runtime AST
apply ast [] = return ast
apply (String s) [Int n] = return $ String [s !! n]
apply (List l) [Int n] = return $ l !! n
apply (Class env) argv = return $ Class $ (zip (map fst env) argv) ++ (drop (length argv) env)
apply (Match conds) argv = miss "no implement"
apply (Func args body) argv = go args
  where
    go [] = return body -- keep pointer for lazy evaluation
    go _ = do
      s <- get
      put $ s { local = new args argv }
      ret <- unify body
      put $ s { local = local s }
      return ret
apply ast argv = miss $ show ast ++ " with " ++ show argv

ref :: String -> Runtime AST
ref name = do
  s <- get
  case lookup name $ (local s) ++ (share s) ++ (global s) of
    Just x -> unify x
    Nothing -> miss $ "Not found: " ++ name

new :: Env -> [AST] -> Env
new env argv = (zip (map fst env) argv) ++ (drop (length argv) env)

throw message = Class [("__throw", String message)]

-- utility
debug name = trace name (return ())

is_throw (Class xs)
  | any (\(k, _) -> k == "__thorw") xs = True
  | otherwise = False
is_throw _ = False

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
string_join :: String -> [String] -> String
string_join glue [] = ""
string_join glue [x] = x
string_join glue (x:xs) = x ++ glue ++ (string_join glue xs)

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

keys env = (string_join ", " $ map fst env)
kvs env = "- " ++ (string_join "\n- " $ map (\(k,v) -> k ++ " " ++ (take 70 $ show v)) env)
showStack n [] = "(empty)"
showStack 0 ((name, s):_) = "# " ++ name ++ "\n" ++ (kvs $ local s)
showStack n (x:xs) = showStack (n - 1) xs

escape [] = ""
escape ('"':xs) = "\\\"" ++ escape xs
escape (x:xs) = x : escape xs

miss :: String -> Runtime AST
miss message = do
  s <- get
  error $ string_join "\n" [
      message
    , ""
    , "Stacks"
    , showStack 0 (stack s)
    , showStack 1 (stack s)
    , showStack 2 (stack s)
    , showStack 3 (stack s)
    , showStack 4 (stack s)
    ]
