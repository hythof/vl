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

unify env (List xs) = List $ map (unify env) xs
unify env (Apply name argv) = dispatch env name (map (unify env) argv)
unify env x = x

dispatch env "_" [] = Apply "_" []
dispatch env "|" [(Throw _), r] = r
dispatch env "|" [l, _] = l
dispatch env "&&" [(Bool True), r] = r
dispatch env "&&" [(Bool False), _] = Bool False
dispatch env "||" [(Bool True), _] = Bool True
dispatch env "||" [(Bool False), r] = r
dispatch env "+" [(Int l), (Int r)] = Int $ l + r
dispatch env "-" [(Int l), (Int r)] = Int $ l - r
dispatch env "*" [(Int l), (Int r)] = Int $ l * r
dispatch env ">" [(Int l), (Int r)] = Bool $ l > r
dispatch env ">=" [(Int l), (Int r)] = Bool $ l >= r
dispatch env "<" [(Int l), (Int r)] = Bool $ l < r
dispatch env "<=" [(Int l), (Int r)] = Bool $ l <= r
dispatch env "." [(String l), (String r)] = String $ l ++ r
dispatch env "++" [(List l), (List r)] = List $ l ++ r
dispatch env "==" [l, r] = Bool $ show l == show r
dispatch env "!=" [l, r] = Bool $ show l /= show r
dispatch env _ (a@(Throw _):_) = a
dispatch env "map"  [List xs, ast] = List $ map (\arg -> unify env $ apply env [arg] ast) xs
dispatch env "has"  [List xs, ast] = Bool $ elem ast xs
dispatch env "join" [List xs, String glue] = String $ string_join glue (map to_string xs)
dispatch env "length" [String s] = Int $ length s
dispatch env "slice"  [String s, Int n] = String $ drop n s
dispatch env "slice"  [String s, Int n, Int m] = String $ take m (drop n s)
dispatch env "at" [String s, Int index] = if index < length s
    then String $ [(s !! index)]
    else Throw $ "out of index " ++ show index ++ " in \"" ++ s ++ "\""
dispatch env name ((Struct fields):argv) = case find "@3" name fields $ apply (fields ++ env) argv of
  Block steps -> Block $ (map (\(k,v) -> Define k v) (reverse fields)) ++ steps
  x -> x
dispatch env name argv = find "@4" name env (apply env argv)

block env [] = Void
block env [ast] = affect env ast
block env (head_ast:rest) = branch head_ast rest
  where
    branch ast rest = case unify env ast of
      a@(Throw _) -> a
      Block steps -> block env $ steps ++ rest
      Define name exp -> block ((name, unify env exp) : env) rest
      Assign name exp -> case unify env exp of
        a@(Throw _) -> a
        Block steps -> block env $ (last_assign name steps []) ++ rest
        _ -> block ((name, unify env exp) : env) rest
      x -> block env rest
    last_assign name [last] acc = reverse $ (Assign name last) : acc
    last_assign name (x:xs) acc = last_assign name xs (x : acc)

apply env argv ast = go (unify env ast)
  where
    go (Func args Void) = Struct $ zip args argv
    go (Func args (Enum name Void)) = Enum name (Struct $ zip args argv)
    go (Func args (Struct kvs)) = Struct $ (zip args argv) ++ kvs
    go (Func args (Match matches)) = match args matches
    go (Func args (Block steps)) = Block $ (map (\(k, v) -> Define k v) (zip args argv)) ++ steps
    go (Func args body) = unify ((zip args argv) ++ env) body
    go v = if length argv == 0 then v else Throw $ "bug:" ++ show v ++ " with " ++ show argv
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
to_string (String s) = '"' : escape s ++ "\""
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
