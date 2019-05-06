module Evaluator where

import Debug.Trace (trace)
import AST

reserved_func = ["length", "slice", "map", "join", "has", "at", "to_int"]
reserved_op2 = ["_", "|" , "|" , "&&" , "&&" , "||" , "||" , "+" , "-" , "*" , ">" , ">=" , "<" , "<=" , "." , "++" , "==" , "!="]

eval :: Env -> AST -> AST
eval env input = affect env (unify env input)

affect env (Apply name argv) = dispatch env name (map (unify env) argv)
affect env (Block steps) = block env steps Void
affect env ast = ast

unify env ast = unify_ env ast
unify_ env (List xs) = List $ map (unify env) xs
unify_ env (Apply name argv) = dispatch env name (map (unify env) argv)
unify_ env x = x

dispatch env name argv = go name argv
  where
    go name argv
      | elem name reserved_op2 = dispatch_op2 name argv
      | elem name reserved_func = dispatch_func name argv
      | otherwise = dispatch_call name argv
    dispatch_op2 "_" [] = Void
    dispatch_op2 "|" [Throw _, r] = r
    dispatch_op2 "|" [l, r] = Block [Apply "|" [l, r]]
    dispatch_op2 "&&" [(Bool True), r] = r
    dispatch_op2 "&&" [(Bool False), _] = Bool False
    dispatch_op2 "||" [(Bool True), _] = Bool True
    dispatch_op2 "||" [(Bool False), r] = r
    dispatch_op2 "+" [(Int l), (Int r)] = Int $ l + r
    dispatch_op2 "-" [(Int l), (Int r)] = Int $ l - r
    dispatch_op2 "*" [(Int l), (Int r)] = Int $ l * r
    dispatch_op2 ">" [(Int l), (Int r)] = Bool $ l > r
    dispatch_op2 ">=" [(Int l), (Int r)] = Bool $ l >= r
    dispatch_op2 "<" [(Int l), (Int r)] = Bool $ l < r
    dispatch_op2 "<=" [(Int l), (Int r)] = Bool $ l <= r
    dispatch_op2 "." [(String l), (String r)] = String $ l ++ r
    dispatch_op2 "++" [(List l), (List r)] = List $ l ++ r
    dispatch_op2 "==" [l, r] = Bool $ show l == show r
    dispatch_op2 "!=" [l, r] = Bool $ show l /= show r
    dispatch_op2 op argv = error $ "op2: " ++ op ++ " " ++ show argv
    dispatch_func _ (a@(Throw _):_) = a
    dispatch_func "map"  [List xs, ast] = List $ map (\arg -> unify env $ apply env [arg] ast) xs
    dispatch_func "has"  [List xs, ast] = Bool $ elem ast xs
    dispatch_func "join" [List xs, String glue] = String $ string_join glue (map to_string xs)
    dispatch_func "to_int" [String s] = Int (read s :: Int)
    dispatch_func "length" [String s] = Int $ length s
    dispatch_func "slice"  [String s, Int n] = String $ drop n s
    dispatch_func "slice"  [String s, Int n, Int m] = String $ take m (drop n s)
    dispatch_func "at" [String s, Int index] = if index < length s
      then String $ [(s !! index)]
      else Throw $ "out of index " ++ show index ++ " in \"" ++ s ++ "\""
    dispatch_func name argv = error $ "func: " ++ name ++ " " ++ show argv
    dispatch_call name ((Struct fields):argv) = case find "call by struct " name fields $ apply (fields ++ env) argv of
      Block steps -> Block $ (map (\(k,v) -> Define k v) (reverse fields)) ++ steps
      x -> x
    --dispatch_call name argv = trace_ name $ find ("call by general " ++ show argv) name env (apply env argv)
    dispatch_call name argv = find ("call by general " ++ show argv) name env (apply env argv)

block env [] last = last
block env (head_ast:rest) last = branch head_ast rest
  where
    branch ast rest = case unify env ast of
      a@(Throw _) -> a
      Block [Apply "|" [l, r]] -> or "__or__" l r
      Block steps -> block env (steps ++ rest) last
      Define name exp -> block ((name, unify env exp) : env) rest (unify env exp)
      Assign name exp -> do_assign name exp
      x -> if length rest == 0 then x else block env rest x
    do_assign name exp = case unify env exp of
      a@(Throw _) -> a
      Block [Apply "|" [l, r]] -> or name l r
      Block steps -> continue name (Block steps)
      x -> block ((name, unify env exp) : env) rest x
    or name l r = case branch l [] of
      Throw _ -> continue name (unify env r)
      Block steps -> error $ "BUG " ++ name ++ "  " ++ show steps
      l -> continue name (unify env l)
    continue :: String -> AST -> AST
    continue name (Block steps) = block env ((last_assign name steps []) ++ rest) Void
    continue name ast@(Throw _) = ast
    continue name ast = block ((name, ast) : env) rest ast
    last_assign :: String -> [AST] -> [AST] -> [AST]
    last_assign name [] _ = error "BUG: empty assign"
    last_assign name [last] acc = reverse $ (Assign name last) : acc
    last_assign name (x:xs) acc = last_assign name xs (x : acc)

apply env argv ast = go (unify env ast)
  where
    go (Func args Void) = Struct $ zip args argv
    go (Func args (Enum name Void)) = Enum name (Struct $ zip args argv)
    go (Func args (Struct kvs)) = Struct $ (zip args argv) ++ kvs
    go (Func args (Match matches)) = match args matches
    go (Func args (Block steps)) = Block $ (map (\(k, v) -> Define k v) (zip args argv)) ++ steps
    go (Func args body) = case unify ((zip args argv) ++ env) body of
      Block steps -> Block $ (zipWith Define args argv) ++ steps
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
    is _ Void = True
    is (Enum t1 v) (Enum t2 _) = t1 == t2
    is v1 v2 = v1 == v2

find debug_mark name env f = case lookup name env of
  Just (Apply name' []) -> if name == name'
    then error $ debug_mark ++ " circle reference " ++ name ++ " in " ++ (show env)
    else find debug_mark name' env f
  Just v -> f v
  _ -> error $ debug_mark ++ " " ++ message
    where
      message = "not found " ++ name ++ " in " ++ (show $ map fst env)

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
trace_ mark x = trace ("* " ++ (show mark) ++ " " ++ show x) x
