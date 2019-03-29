module Evaluator where

import Debug.Trace (trace)
import AST

eval :: Env -> AST -> AST
eval env a@(Void) = a
eval env a@(Bool _) = a
eval env a@(Int _) = a
eval env a@(String _) = a
eval env (List xs) = List $ map (\x -> eval env x) xs
eval env (Ref name) = find name env id
eval env (Apply body []) = eval env body
eval env (Apply body argv) = apply env argv body
eval env a@(Struct _) = a
eval env a@(Match _) = a
eval env a@(Throw _) = a
eval env a@(Return _) = a
eval env a@(Enum _ _) = a
eval env (Func [] a) = eval env a
eval env a@(Func _ _) = a
eval env (Op2 op left right) = go op (eval env left) (eval env right)
  where
    go "|" (Throw _) r = r
    go "|" l _ = l
    go "&&" (Bool True) r = r
    go "&&" (Bool False) _ = Bool False
    go "||" (Bool True) _ = Bool True
    go "||" (Bool False) r = r
    go "+" (Int l) (Int r) = Int $ l + r
    go "-" (Int l) (Int r) = Int $ l - r
    go "*" (Int l) (Int r) = Int $ l * r
    go ">" (Int l) (Int r) = Bool $ l > r
    go ">=" (Int l) (Int r) = Bool $ l >= r
    go "<" (Int l) (Int r) = Bool $ l < r
    go "<=" (Int l) (Int r) = Bool $ l <= r
    go "." (String l) (String r) = String $ l ++ r
    go "==" l r = Bool $ show l == show r
    go "!=" l r = Bool $ show l /= show r
    go op l r = Throw $ "op: (" ++ show l ++ ") " ++ op ++ " (" ++ show r ++ ")"
eval env a@(Method self method argv_) = go (eval env self) method (map (eval env) argv_)
  where
    go (Struct fields) _ argv = find method fields $ apply (fields ++ env) argv
    go (String s) "length" [] = Int $ length s
    go (String s) "slice" [Int n] = String $ drop n s
    go (String s) "slice" [Int n, Int m] = String $ take m (drop n s)
    go (String s) ns [] = case is_num of
        False -> Throw $ "method not found: String." ++ ns
        True
          | index < length s -> String $ [(s !! index)]
          | otherwise -> Throw $ "out of index " ++ s ++ "." ++ ns
      where
        is_num = all (\x -> elem x "01234566789") ns
        index = if is_num then read ns :: Int else -1
    go a@(Throw _) _ _ = a
    go v _ _ = Throw $ "method " ++ show v ++
             "." ++ method ++
             "(" ++ (show argv_) ++ ")"
eval env (Steps root_step) = go root_step
  where
    go :: [AST] -> AST
    go [] = Void
    go [ast] = eval env ast
    go (ast:rest) = branch ast rest (\ast ->
      branch (eval env ast) rest (\ast ->
        eval env $ Steps rest))
    branch ast rest f = case ast of
      Return ret -> ret
      Throw _ -> ast
      Assign name exp -> case eval env exp of
        a@(Throw _) -> a
        _ -> eval ((name, eval env exp):env) (Steps rest)
      _ -> f ast

eval env ast = error $ "unknown " ++ show ast

-- utility
find name env f = case lookup name env of
  Just (Ref name') -> if name == name'
    then Throw $ "circle reference " ++ name ++ " in " ++ (show env)
    else find name' env f
  Just v -> f $ eval env v
  _ -> Throw $ "not found " ++ name ++ " in " ++ (show $ map fst env)

apply env argv_ ast = go (eval env ast)
  where
    argv = map (eval env) argv_
    go (Func args Void) = Struct $ zip args argv
    go (Func args (Enum name Void)) = Enum name (Struct $ zip args argv)
    go (Func args (Struct kvs)) = Struct $ (zip args argv) ++ kvs
    go (Func args (Match matches)) = match env (args !! 0) (argv !! 0) matches
    go (Func args body) = eval ((zip args argv) ++ env) body
    go (Ref name) = find name env id
    go v = eval env v
    match env name a@(Enum t1 v) ((EnumPattern t2, ast):rest) = if t1 == t2
      then eval ((name, v) : env) ast
      else match env name a rest
    match _ _ _ _ = Throw $ "miss match " ++ show argv ++ show (eval env ast)
