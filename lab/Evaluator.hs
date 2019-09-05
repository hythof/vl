module Evaluator where

import Debug.Trace (trace)
import AST
import Parser (parseAST)
import Control.Monad (filterM)
import Data.Foldable (find)
import System.IO.Unsafe (unsafePerformIO)

eval :: Env -> AST -> AST
eval env top = evalRuntime (unify top) (Scope env [] [] [] [] [] [] [])

-- private functions

unify :: AST -> Runtime AST
unify (List xs) = List <$> mapM unify xs
unify (Block lines) = do
  s1 <- get
  put $ s1 { block = [] }
  v <- fmap last $ mapM unwrap lines
  modify $ \s2 -> s2 { block = block s1 }
  return v
unify (Assign name body) = do
  ret <- unwrap body
  modify $ \s -> s { block = squash [(name, ret)] (block s) }
  return ret
unify (Update name body) = do
  ret <- unwrap body
  modify $ \s -> s { vars = squash [(name, ret)] (vars s) }
  return ret
unify (Call name argv) = go
  where
    go = do
      s1 <- get
      let label = name ++ " : " ++ (keys $ local s1 ++ block s1)
      put $ s1 { stack = (label, s1) : (stack s1) }
      ret <- switch
      modify $ \s2 -> s2 { stack = stack s1, history = (name, argv, ret) : history s2 }
      return ret
    l = argv !! 0
    r = argv !! 1
    switch = case name of
      "|" -> do
        s <- get
        unwrap l <|> (put s >> unwrap r)
      "||" -> do
        unwrap l >>= \l' -> case l' of
          Bool True -> return $ Bool True
          Bool False -> unify r
          _ -> throw $ "|| " ++ show l'
      "&&" ->
        unwrap l >>= \l' -> case l' of
          Bool True -> unify r
          Bool False -> return $ Bool False
          _ -> throw $ "invalid bool && " ++ show l'
      _ -> do
        argv <- mapM unify argv
        call name argv
unify (Func args [] body) = do
  s <- get
  let env = squash (block s) (local s)
  return $ Func args env body
unify ast = return ast

unwrap ast = unify ast >>= go
  where
    go (Func [] env body) = do
      s1 <- get
      put $ s1 { local = env }
      ret <- unify body
      modify $ \s2 -> s2 { local = local s1 }
      unwrap ret
    go (Throw message) = throw message
    go ast = return ast

call :: String -> [AST] -> Runtime AST
call "_" [] = return Void
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
call "has"  [List xs, ast] = trace ("has" ++ show ast ++ " " ++ (show $ length xs) ++ " => " ++ show (elem ast xs)) $ return $ Bool (elem ast xs)
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
call "at" [String s, Int index] = if index < length s
  then return $ String $ [(s !! index)]
  else throw $ "out of index " ++ show index ++ " in \"" ++ s ++ "\""
call "at" [List s, Int index] = if index < length s
  then return $ s !! index
  else throw $ "out of index " ++ show index ++ " in \"" ++ show s ++ "\""
call "throw" argv = throw $ string_join ", " $ map to_string argv
call "bp" [ast] = go
  where
    go = do
      s <- get
      return $ unsafePerformIO $ do
        putStrLn $ "Brake point: " ++ show ast
        putStrLn $ dump s
        loop
    loop = do
      putStr "> "
      cmd <- getLine
      case cmd of
        "quit" -> return Void
        "exit" -> return Void
        "q" -> return Void
        _ -> loop

call name ((Class props env):argv) = do
  s1 <- get
  vs <- new props argv
  put $ s1 { vars = vs, methods = env }
  ret <- call name argv
  modify $ \s2 -> s2 { vars = vars s1, methods = methods s1 }
  return $ ret
call name argv = do
  ast <- ref name argv
  apply ast argv

apply :: AST -> [AST] -> Runtime AST
apply ast [] = return ast
apply (String s) [Int n] = return $ String [s !! n]
apply (List l) [Int n] = return $ l !! n
apply (Class props env) argv = (new props argv) >>= \vs -> return $ Class vs env
apply (Match conds) argv = miss "no implement"
apply (Func args env body) argv = do
  s1 <- get
  put $ s1 { local = squash3 (zip args argv) env (local s1) }
  ret <- unify body
  modify $ \s2 -> s2 { local = local s1 }
  return ret
apply ast argv = miss $ show ast ++ " with " ++ show argv

ref :: String -> [AST] -> Runtime AST
ref name argv = do
  s <- get
  case lookup name $ (local s) ++ (block s) ++ (vars s) ++ (methods s) ++ (global s) of
    Just x -> unify x
    Nothing -> miss $ "Not found: " ++ name ++ " with " ++ show argv

new :: Env -> [AST] -> Runtime Env
new env argv = if length env == length argv
  then return $ zip (map fst env) argv
  else miss $ "Argument miss match " ++ show env ++ " " ++ show argv

squash :: Env -> Env -> Env
squash [] ys = ys
squash xs [] = xs
squash xs ys = go ys []
  where
    go [] acc = xs ++ (reverse acc)
    go (y@(k,_):ys) acc = if any (\(k',_) -> k == k') xs
      then go ys acc
      else go ys (y : acc)

squash3 xs ys zs = squash xs (squash ys zs)

throw message = Runtime $ \s -> Left (Throw message, s { throws = message : throws s })

l <|> r = go
  where
    go = Runtime $ \s -> case runState l s of
      Left (e, s') -> runState r (s' { vars = vars s, local = local s, throws = handleLast $ throws s })
      l' -> l'
    handleLast [] = []
    handleLast (x:xs) = ("catched " ++ x) : xs

-- utility
miss :: String -> Runtime a
miss message = do
  s <- get
  error $ message ++ "\n" ++ dump s
