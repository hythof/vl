module Evaluator where

import Debug.Trace (trace)
import AST

reserved_func = ["trace", "not", "length", "slice", "find", "map", "join", "has", "at", "to_int", "to_float", "to_string"]
reserved_op2 = ["_", "|", "&&" , "||" , "+" , "-" , "*" , "/" , ">" , ">=" , "<" , "<=" , "." , "++" , "==" , "!="]

eval :: Env -> AST -> AST
eval env input = affect env (unify env input)

affect env (Apply name argv) = affect env $ dispatch env name (map (unify env) argv)
affect env (Block steps) = fst $ block env [] steps Void
affect env ast = ast

unify env ast = unify_ env ast
unify_ env (List xs) = List $ map (unify env) xs
unify_ env (Apply name argv) = dispatch env name (map (unify env) argv)
unify_ env x = x

dispatch env name argv = go name argv
  where
    go name argv
      | elem name reserved_op2 = dispatch_op name argv
      | elem name reserved_func = dispatch_func name argv
      | otherwise = dispatch_call name argv
    dispatch_op "_" [] = Void
    dispatch_op "|" [Throw _, r] = r
    dispatch_op "|" [l, r] = Block [Apply "|" [l, r]]
    dispatch_op "||" [(Bool True), _] = Bool True
    dispatch_op "||" [(Bool False), r] = r
    dispatch_op op [l@(Throw _), _] = l
    dispatch_op op [_, r@(Throw _)] = r
    dispatch_op op [l, r] = dispatch_op2 op [l, r]
    dispatch_op2 "&&" [(Bool True), r] = r
    dispatch_op2 "&&" [(Bool False), _] = Bool False
    dispatch_op2 "+" [(Int l), (Int r)] = Int $ l + r
    dispatch_op2 "-" [(Int l), (Int r)] = Int $ l - r
    dispatch_op2 "*" [(Int l), (Int r)] = Int $ l * r
    dispatch_op2 "/" [(Int l), (Int r)] = Int $ div l r
    dispatch_op2 ">" [(Int l), (Int r)] = Bool $ l > r
    dispatch_op2 ">=" [(Int l), (Int r)] = Bool $ l >= r
    dispatch_op2 "<" [(Int l), (Int r)] = Bool $ l < r
    dispatch_op2 "<=" [(Int l), (Int r)] = Bool $ l <= r
    dispatch_op2 "." [(String l), (String r)] = String $ l ++ r
    dispatch_op2 "++" [(List l), (List r)] = List $ l ++ r
    dispatch_op2 "==" [l, r] = Bool $ dispatch_eq l r
    dispatch_op2 "!=" [l, r] = Bool $ not $ dispatch_eq l r
    dispatch_op2 op argv = error $ "op2: " ++ op ++ " " ++ show argv
    dispatch_eq Void Void = True
    dispatch_eq (Bool a) (Bool b) = a == b
    dispatch_eq (Int a) (Int b) = a == b
    dispatch_eq (String a) (String b) = a == b
    dispatch_eq (List a) (List b) = all id (zipWith dispatch_eq a b)
    dispatch_eq (Struct a) (Struct b) = show a == show b
    dispatch_eq a@(Enum _ _) b@(Enum _ _) = show a == show b
    dispatch_eq a@(Throw _) b@(Throw _) = show a == show b
    dispatch_eq a b = error $ "equal: " ++ show a ++ " == " ++ show b
    dispatch_func _ (a@(Throw _):_) = a
    dispatch_func "trace" args = trace ("TRACE: " ++ show args) Void
    dispatch_func "map"  [List xs, ast] = List $ map (\arg -> unify env $ apply "" env [arg] ast) xs
    dispatch_func "find"  [List xs, ast] = (filter (\x -> (apply "__find__" env [x] ast) == Bool True) xs) !! 0
    dispatch_func "has"  [List xs, ast] = Bool $ elem ast xs
    dispatch_func "join" [List xs, String glue] = String $ string_join glue (map to_string xs)
    dispatch_func "to_int" [String s] = Int (read s :: Int)
    dispatch_func "to_float" [String s] = Float (read s :: Double)
    dispatch_func "to_string" [Int n] = String $ show n
    dispatch_func "to_string" [Float n] = String $ show n
    dispatch_func "to_string" [String s] = String $ '"' :  s ++ "\""
    dispatch_func "not" [Bool b] = Bool $ not b
    dispatch_func "length" [String s] = Int $ length s
    dispatch_func "slice"  [String s, Int n] = String $ drop n s
    dispatch_func "slice"  [String s, Int n, Int m] = String $ take m (drop n s)
    dispatch_func "at" [String s, Int index] = if index < length s
      then String $ [(s !! index)]
      else Throw $ "out of index " ++ show index ++ " in \"" ++ s ++ "\""
    dispatch_func "at" [List s, Int index] = if index < length s
      then s !! index
      else Throw $ "out of index " ++ show index ++ " in \"" ++ show s ++ "\""
    dispatch_func name argv = error $ "func: " ++ name ++ " " ++ show argv
    dispatch_call name ((Flow props throws fields):argv) = go
      where
        local = (zip props argv) ++ throws ++ fields ++ env
        go = if elem name (map fst throws) then go_throw else go_method
        go_throw = find name throws
        go_method = if (length props) <= (length argv)
          then case find name local of
            Func args body -> if (length args) == (length argv) - (length props)
              then affect ((zip args (drop (length props) argv)) ++ local) body
              else error $ "Too many arguments " ++ name ++ " have " ++ show props ++ " but args " ++ show argv
            x -> if (length props) == (length argv)
              then affect local x
              else error $ "Too many arguments '" ++ name ++ "' have " ++ show props ++ " but args " ++ show argv
          else error $ "Few arguments " ++ name ++ " have " ++ show props ++ " but args " ++ show argv
    dispatch_call name (s@(Struct fields):argv) = case lookup name fields of
      Just body -> apply name (fields ++ env) argv body
      _ -> apply name env (s : argv) $ find name env
    dispatch_call name argv = apply name env argv $ find name env

block :: Env -> Env -> [AST] -> AST -> (AST, Env)
block env local _ ast@(Throw _) = (ast, local)
block env local [] ast = (ast, local)
block env local (head_ast:rest) last = branch head_ast rest
  where
    uni :: AST -> AST
    uni ast = unify (local ++ env) ast
    branch :: AST -> [AST] -> (AST, Env)
    branch ast rest = case uni ast of
      a@(Throw _) -> (a, local)
      Block [Apply "|" [l, r]] -> case branch l [] of
        (Throw _, _) -> branch r rest
        _ -> branch l rest
      Block steps -> let (ast, local2) = block (local ++ env) [] steps Void in block env (local2 ++ local) rest ast
      Define name exp -> block ((name, uni exp) : env) local rest Void
      Assign name exp -> let (ast, local2) = branch exp [] in block ((name, ast) : env) (local2 ++ local) rest ast
      Update name exp -> let (ast, local2) = branch exp [] in block env ((name, ast) : local2 ++ local) rest ast
      ast -> block env local rest ast

find name env = case lookup name env of
  Just (Apply name' []) -> if name == name'
    then error $ "Circle reference " ++ name ++ " in " ++ (show $ map fst env)
    else find name' env
  --Just v@(Throw x) -> trace ("throw: " ++ name ++ show env) $ v
  Just v -> v
  _ -> error $ "Not found '" ++ name ++ "' in " ++ (show $ map fst env)

apply name env argv ast = go (unify env ast)
  where
    go (Func args body) = if (length argv) == (length args)
      then run args body
      else error $ "Miss match " ++ name ++ " (" ++ (show $ length args) ++ " != " ++ (show $ length argv) ++
        " in " ++ show args ++ " != " ++ show argv ++ " => " ++ show body
    go v = if length argv == 0 then v else error $ "APPLY: " ++ name ++ " " ++ show v ++ "\nwith " ++ show argv
    run args Void = Struct $ zip args argv
    run args (Enum name Void) = Enum name (Struct $ zip args argv)
    run args (Struct kvs) = Struct $ (zip args argv) ++ kvs
    run args (Match matches) = match args matches
    run args (Block steps) = Block $ (map (\(k, v) -> Define k v) (zip args argv)) ++ steps
    run args body = case unify ((zip args argv) ++ env) body of
      Block steps -> Block $ (zipWith Define args argv) ++ steps
      Func args2 body2 -> Func args2 (Apply "_apply" [Struct (("_apply", body2) : (zip args argv))])
      result -> result
    match :: [String] -> [([AST], AST)] -> AST
    match args all_conds = if (length argv) == (length args)
      then match_ args all_conds
      else error $ "Unmatch " ++ name ++ " " ++ show args ++ " != " ++ show argv
      where
        match_ args [] = error $ "miss match\ntarget: " ++ show argv ++ "\ncase: " ++ string_join "\ncase: " (map (show . fst) all_conds)
        match_ args ((conds,branch):rest) = if (length argv) == (length conds)
          then if all id (zipWith is argv (map (unify env) conds))
            then unify ((zip args (map unwrap argv)) ++ env) branch
            else match_ args rest
          else error $ "Unmatch " ++ name ++ " " ++ show args ++ " have " ++ show argv ++ " != " ++ show conds ++ " => " ++ show branch
    unwrap (Enum _ v) = v
    unwrap v = v
    is _ Void = True
    is (Enum t1 _) (Enum t2 _) = t1 == t2
    is v1 v2 = v1 == v2

-- utility
to_strings xs = string_join " " (map to_string xs)
to_string Void = "_"
to_string (Bool True) = "true"
to_string (Bool False) = "false"
to_string (Int n) = show n
to_string (Float n) = show n
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
to_string (Update name ast) = name ++ " := " ++ to_string ast
escape [] = ""
escape ('"':xs) = "\\\"" ++ escape xs
escape (x:xs) = x : escape xs
string_join :: String -> [String] -> String
string_join glue [] = ""
string_join glue [x] = x
string_join glue (x:xs) = x ++ glue ++ (string_join glue xs)
trace_ mark x = trace ("* " ++ (show mark) ++ " " ++ show x) x
