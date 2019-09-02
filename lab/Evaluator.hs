module Evaluator where

import Debug.Trace (trace)
import AST
import Parser (parseAST)
import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.Foldable (find)

eval :: Env -> AST -> AST
eval env top = evalRuntime (unify top) (Scope env [] [] [])

-- private functions

unify :: AST -> Runtime AST
unify (List xs) = List <$> mapM unify xs
unify (Block lines) = fmap last $ mapM unify lines
unify (Assign name body) = do
  ret <- unify body
  ret <- unwrap ret
  modify (\s -> s { local = (name, ret) : local s })
  return ret
unify (Update name body) = do
  ret <- unify body
  ret <- unwrap ret
  modify (\s -> s { share = (name, ret) : (share s) })
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
  s1 <- get
  put $ s1 { local = args ++ local s1 }
  ret <- unify body
  modify $ \s2 -> s2 { local = local s1 }
  return ret
unwrap ast = return ast

callWithStack :: String -> [AST] -> Runtime AST
callWithStack name argv = do
  s1 <- get
  let label = name ++ " : " ++ (keys $ local s1)
  put $ s1 { stack = (label, s1) : stack s1 }
  ret <- call name argv
  modify $ \s2 -> s2 { stack = stack s1 }
  return ret
call :: String -> [AST] -> Runtime AST
call "_" [] = return Void
call "|" [l, r] = return $ if is_throw l then l else r
call "||" [(Bool True), _] = return $ Bool True
call "||" [(Bool False), r] = return r
call "||" args = error $ show args
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
  modify $ \s2 -> s2 { share = share s1 }
  return $ ret
call name argv = do
  ast <- ref name
  apply ast argv

apply :: AST -> [AST] -> Runtime AST
apply (String s) [Int n] = return $ String [s !! n]
apply (List l) [Int n] = return $ l !! n
apply (Class env) argv = return $ Class $ (zip (map fst env) argv) ++ (drop (length argv) env)
apply (Match conds) argv = miss "no implement"
apply (Func args body) argv = do
  s1 <- get
  put $ s1 { local = (new args argv) ++ local s1 }
  ret <- unify body
  modify $ \s2 -> s2 { local = local s1 }
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
debug x = trace (take 70 $ show x) (return ())

is_throw (Class xs)
  | any (\(k, _) -> k == "__throw") xs = True
  | otherwise = False
is_throw _ = False

miss :: String -> Runtime AST
miss message = do
  s <- get
  error $ message ++ dump s
