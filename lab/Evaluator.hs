module Evaluator where

import Debug.Trace (trace)
import AST

reserved_func = ["length", "slice", "map", "join", "has", "at"]
reserved_op2 = ["|" , "|" , "&&" , "&&" , "||" , "||" , "+" , "-" , "*" , ">" , ">=" , "<" , "<=" , "." , "++" , "==" , "!="]

eval :: Env -> AST -> AST
eval env input = go_top input
  where
    go_top (Block root_step) = block env root_step
    go_top x = go x
    go (List xs) = List $ map go xs
    go (Apply name argv) = run name (map go argv)
    go (Func [] a) = go a
    go x = x
    run "_" [] = Apply "_" []
    run name [] = find "@1" name env id
    run "|" [(Throw _), r] = r
    run "|" [l, _] = l
    run "&&" [(Bool True), r] = r
    run "&&" [(Bool False), _] = Bool False
    run "||" [(Bool True), _] = Bool True
    run "||" [(Bool False), r] = r
    run "+" [(Int l), (Int r)] = Int $ l + r
    run "-" [(Int l), (Int r)] = Int $ l - r
    run "*" [(Int l), (Int r)] = Int $ l * r
    run ">" [(Int l), (Int r)] = Bool $ l > r
    run ">=" [(Int l), (Int r)] = Bool $ l >= r
    run "<" [(Int l), (Int r)] = Bool $ l < r
    run "<=" [(Int l), (Int r)] = Bool $ l <= r
    run "." [(String l), (String r)] = String $ l ++ r
    run "++" [(List l), (List r)] = List $ l ++ r
    run "==" [l, r] = Bool $ show l == show r
    run "!=" [l, r] = Bool $ show l /= show r
    run _ (a@(Throw _):_) = a
    run "map"  [List xs, ast] = List $ map (\arg -> go $ apply env [arg] ast) xs
    run "has"  [List xs, ast] = Bool $ elem ast xs
    run "join" [List xs, String glue] = String $ string_join glue (map to_string xs)
    run "length" [String s] = Int $ length s
    run "slice"  [String s, Int n] = String $ drop n s
    run "slice"  [String s, Int n, Int m] = String $ take m (drop n s)
    run "at" [String s, Int index] = if index < length s
        then String $ [(s !! index)]
        else Throw $ "out of index " ++ show index ++ " in \"" ++ s ++ "\""
    run name ((Struct fields):argv) = find "@3" name fields $ apply (fields ++ env) argv
    run name argv = find "@4" name env (apply env argv)

    block env [] = Void
    block env [ast] = eval env ast
    block env (head_ast:rest) = branch head_ast rest
      where
        branch ast rest = case eval env ast of
          a@(Throw _) -> a
          Assign name exp -> case eval env exp of
            a@(Throw _) -> a
            _ -> block ((name, go exp):env) rest
          x -> block env rest

apply env argv ast = go body
  where
    body = eval env ast
    go (Func args Void) = Struct $ zip args argv
    go (Func args (Enum name Void)) = Enum name (Struct $ zip args argv)
    go (Func args (Struct kvs)) = Struct $ (zip args argv) ++ kvs
    go (Func args (Match matches)) = match args matches
    go (Func args body) = eval ((zip args argv) ++ env) body
    go v = eval env v
    match :: [String] -> [([AST], AST)] -> AST
    match args all_conds = match_ args all_conds
      where
        match_ args [] = Throw $ "miss match\ntarget: " ++ show argv ++ "\ncase: " ++ string_join "\ncase: " (map (show . fst) all_conds)
        match_ args ((conds,branch):rest) = if all id (zipWith is argv (map (eval env) conds))
          then eval ((zip args (map unwrap argv)) ++ env) branch
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
to_string (Assign name ast) = name ++ " = " ++ to_string ast
escape [] = ""
escape ('"':xs) = "\\\"" ++ escape xs
escape (x:xs) = x : escape xs
string_join :: String -> [String] -> String
string_join glue [] = ""
string_join glue [x] = x
string_join glue (x:xs) = x ++ glue ++ (string_join glue xs)
