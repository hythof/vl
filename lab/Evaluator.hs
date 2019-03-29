module Evaluator where

import Debug.Trace (trace)
import AST

eval :: Env -> AST -> AST
eval env input = go input
  where
    go (Void) = input
    go (Bool _) = input
    go (Int _) = input
    go (String _) = input
    go (List xs) = List $ map (\x -> eval env x) xs
    go (Ref name) = find name env id
    go (Apply body []) = eval env body
    go (Apply body argv) = apply env argv body
    go (Struct _) = input
    go (Match _) = input
    go (Throw _) = input
    go (Return _) = input
    go (Enum _ _) = input
    go (Func [] a) = eval env a
    go (Func _ _) = input
    go (Op2 op left right) = op2 op (eval env left) (eval env right)
    go (Method self name argv) = method (eval env self) name (map (eval env) argv)
    go (Steps root_step) = steps root_step
    go _ = error $ "unknown " ++ show input

    op2 "|" (Throw _) r = r
    op2 "|" l _ = l
    op2 "&&" (Bool True) r = r
    op2 "&&" (Bool False) _ = Bool False
    op2 "||" (Bool True) _ = Bool True
    op2 "||" (Bool False) r = r
    op2 "+" (Int l) (Int r) = Int $ l + r
    op2 "-" (Int l) (Int r) = Int $ l - r
    op2 "*" (Int l) (Int r) = Int $ l * r
    op2 ">" (Int l) (Int r) = Bool $ l > r
    op2 ">=" (Int l) (Int r) = Bool $ l >= r
    op2 "<" (Int l) (Int r) = Bool $ l < r
    op2 "<=" (Int l) (Int r) = Bool $ l <= r
    op2 "." (String l) (String r) = String $ l ++ r
    op2 "==" l r = Bool $ show l == show r
    op2 "!=" l r = Bool $ show l /= show r
    op2 op l r = Throw $ "op: (" ++ show l ++ ") " ++ op ++ " (" ++ show r ++ ")"

    method (Struct fields) name argv = find name fields $ apply (fields ++ env) argv
    method (String s) "length" [] = Int $ length s
    method (String s) "slice" [Int n] = String $ drop n s
    method (String s) "slice" [Int n, Int m] = String $ take m (drop n s)
    method (String s) ns [] = case (is_num, index < length s) of
        (False, _) -> Throw $ "method not found: String." ++ ns
        (True, True) -> String $ [(s !! index)]
        _ -> Throw $ "out of index " ++ s ++ "." ++ ns
      where
        is_num = all (\x -> elem x "01234566789") ns
        index = if is_num then read ns :: Int else -1
    method a@(Throw _) _ _ = a
    method v name argv = Throw $ "method " ++ show v ++
      "." ++ name ++
      "(" ++ (show argv) ++ ")"

    steps [] = Void
    steps [ast] = eval env ast
    steps (ast:rest) = branch ast rest (\ast ->
      branch (eval env ast) rest (\ast ->
        eval env $ Steps rest))
      where
        branch ast rest f = case ast of
          Return ret -> ret
          Throw _ -> ast
          Assign name exp -> case eval env exp of
            a@(Throw _) -> a
            _ -> eval ((name, eval env exp):env) (Steps rest)
          _ -> f ast

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
