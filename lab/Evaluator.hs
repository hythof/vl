module Evaluator where

import Debug.Trace (trace)
import AST

reserved_func = ["length", "slice", "map", "join", "has", "at"]
reserved_op2 = ["|" , "|" , "&&" , "&&" , "||" , "||" , "+" , "-" , "*" , ">" , ">=" , "<" , "<=" , "." , "++" , "==" , "!="]

eval :: Env -> AST -> AST
eval env input = affect env (unify env input)

affect env (Apply name argv) = dispatch env name (map (unify env) argv)
affect env (Block steps) = block env steps
affect env ast = ast

unify env ast = unify_ env ast
unify_ env (List xs) = List $ map (unify env) xs
unify_ env (Apply name argv) = dispatch env name (map (unify env) argv)
unify_ env x = x

dispatch env name argv = go name argv
  where
    go "_" [] = Apply "_" []
    go "|" [Throw _, r] = r
    go "|" [l, r] = Apply "|" [l, r]
    go "&&" [(Bool True), r] = r
    go "&&" [(Bool False), _] = Bool False
    go "||" [(Bool True), _] = Bool True
    go "||" [(Bool False), r] = r
    go "+" [(Int l), (Int r)] = Int $ l + r
    go "-" [(Int l), (Int r)] = Int $ l - r
    go "*" [(Int l), (Int r)] = Int $ l * r
    go ">" [(Int l), (Int r)] = Bool $ l > r
    go ">=" [(Int l), (Int r)] = Bool $ l >= r
    go "<" [(Int l), (Int r)] = Bool $ l < r
    go "<=" [(Int l), (Int r)] = Bool $ l <= r
    go "." [(String l), (String r)] = String $ l ++ r
    go "++" [(List l), (List r)] = List $ l ++ r
    go "==" [l, r] = Bool $ show l == show r
    go "!=" [l, r] = Bool $ show l /= show r
    go _ (a@(Throw _):_) = a
    go "map"  [List xs, ast] = List $ map (\arg -> unify env $ apply env [arg] ast) xs
    go "has"  [List xs, ast] = Bool $ elem ast xs
    go "join" [List xs, String glue] = String $ string_join glue (map to_string xs)
    go "to_int" [String s] = Int (read s :: Int)
    go "length" [String s] = Int $ length s
    go "slice"  [String s, Int n] = String $ drop n s
    go "slice"  [String s, Int n, Int m] = String $ take m (drop n s)
    go "at" [String s, Int index] = if index < length s
      then String $ [(s !! index)]
      else Throw $ "out of index " ++ show index ++ " in \"" ++ s ++ "\""
    go name ((Struct fields):argv) = case find "@1" name fields $ apply (fields ++ env) argv of
      Block steps -> Block $ (map (\(k,v) -> Define k v) (reverse fields)) ++ steps
      x -> x
    go name argv = find ("@2 " ++ show argv) name env (apply env argv)

block env [] = Void
block env [ast] = affect env ast
block env (head_ast:rest) = branch head_ast rest
  where
    branch ast rest = case unify env ast of
      a@(Throw _) -> a
      Block steps -> block env $ steps ++ rest
      Define name exp -> block ((name, unify env exp) : env) rest
      Assign name exp -> do_assign name exp
      x -> block env rest
    do_assign name exp = case unify env exp of
      a@(Throw _) -> a
      Apply "|" [l, r] -> or name (unify env l) r
      Block steps -> continue name (Block steps) id
      _ -> block ((name, unify env exp) : env) rest
    or name (Throw _) r = continue name (unify env r) id
    or name (Block steps) r = case block env $ steps ++ rest of
      Throw _ -> continue name r id
      ast -> ast
    or name l _ = block ((name, l) : env) rest
    continue :: String -> AST -> (AST -> AST) -> AST
    continue name (Block steps) f = block env $ (last_assign f name steps []) ++ rest
    continue name ast@(Throw _) _ = ast
    continue name ast _ = block ((name, ast) : env) rest
    last_assign :: (AST -> AST) -> String -> [AST] -> [AST] -> [AST]
    last_assign f name [last] acc = reverse $ (Assign name (f last)) : acc
    last_assign f name (x:xs) acc = last_assign f name xs (x : acc)

apply env argv ast = go (unify env ast)
  where
    go (Func args Void) = Struct $ zip args argv
    go (Func args (Enum name Void)) = Enum name (Struct $ zip args argv)
    go (Func args (Struct kvs)) = Struct $ (zip args argv) ++ kvs
    go (Func args (Match matches)) = match args matches
    go (Func args (Block steps)) = Block $ (map (\(k, v) -> Define k v) (zip args argv)) ++ steps
    go (Func args body) = case unify ((zip args argv) ++ env) body of
      Block steps -> Block $ (map (\(k, v) -> Define k v) (zip args argv)) ++ steps
      Func args2 body2 -> Func args2 (Apply "_apply" [Struct (("_apply", body2) : (zip args argv))])
      result -> result
    go v = if length argv == 0 then v else error $ "bug:" ++ show v ++ " with " ++ show argv
    match :: [String] -> [([AST], AST)] -> AST
    match args all_conds = match_ args all_conds
      where
        match_ args [] = Throw $ "miss match\ntarget: " ++ show argv ++ "\ncase: " ++ string_join "\ncase: " (map (show . fst) all_conds)
        match_ args ((conds,branch):rest) = if all id (zipWith is argv (map (unify env) conds))
          then unify ((zip args (map unwrap argv)) ++ env) branch
          else match_ args rest
    unwrap (Enum _ v) = v
    unwrap v = v
    is _ (Apply "_" []) = True
    is (Enum t1 v) (Enum t2 _) = t1 == t2
    is v1 v2 = v1 == v2

find debug_mark name env f = case lookup name env of
  Just (Apply name' []) -> if name == name'
    then Throw $ debug_mark ++ " circle reference " ++ name ++ " in " ++ (show env)
    else find debug_mark name' env f
  Just v -> f v
  _ -> Throw $ debug_mark ++ " not found " ++ name ++ " in " ++ (show $ map fst env)

-- utility
to_strings xs = string_join " " (map to_string xs)
to_string Void = "_"
to_string (Bool True) = "true"
to_string (Bool False) = "false"
to_string (Int n) = show n
to_string (String s) = s
to_string (List xs) = '[' : (to_strings xs) ++ "]"
to_string (Apply name args) = name ++ "(" ++ to_strings args ++ ")"
to_string (Match matches) = "\n" ++ string_join "\n" (map go matches)
  where
    go (conds, branch) = "| " ++ to_strings conds ++ " = " ++ to_string branch
to_string (Struct kvs) = string_join "\n" (map go kvs)
  where
    go (k, v) = k ++ ":" ++ to_string v
to_string (Enum tag Void) = tag
to_string (Enum tag body) = tag ++ "(" ++ to_string body ++ ")"
to_string (Func args body) = "(" ++ string_join " " args ++ " => " ++ to_string body ++ ")"
to_string (Block block) = "block:" ++ (string_join "\n  " $ map to_string block)
to_string (Throw s) = "throw:" ++ s
to_string (Define name ast) = name ++ " = " ++ to_string ast
to_string (Assign name ast) = name ++ " <- " ++ to_string ast
escape [] = ""
escape ('"':xs) = "\\\"" ++ escape xs
escape (x:xs) = x : escape xs
string_join :: String -> [String] -> String
string_join glue [] = ""
string_join glue [x] = x
string_join glue (x:xs) = x ++ glue ++ (string_join glue xs)
