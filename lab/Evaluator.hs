module Evaluator where

import Debug.Trace (trace)
import AST

reserved_func = ["sub", "rsub1", "if", "trace", "not", "length", "slice", "find", "map", "mapi", "join", "has", "at", "to_int", "to_float", "to_string"]
reserved_op2 = ["_", "|", "&&" , "||" , "+" , "-" , "*" , "/" , ">" , ">=" , "<" , "<=" , "." , "++" , "==" , "!="]

data Scope = Scope {
    global :: Env
  , local :: Env
  , stack :: [String]
} deriving (Show, Eq)

whole :: Scope -> Env
whole scope = (local scope) ++ (global scope)

push :: Scope -> String -> AST -> Scope
push scope name ast = scope { local = (name, ast) : (local scope) }

pushs :: Scope -> Env -> Scope
pushs scope env = scope { local = env ++ (local scope) }

into :: Scope -> String -> Scope
into scope name = scope { stack = name : (stack scope) }

eval :: Env -> AST -> AST
eval env input = affect scope (unify scope input)
  where
    scope = Scope env [] ["main"]

miss :: Scope -> String -> a
miss scope message = error $ message ++ "\nStack: " ++ (string_join " <- " $ stack scope)

affect scope (Apply name argv) = affect scope $ dispatch scope name (map (unify scope) argv)
affect scope (Block steps) = fst $ block scope [] steps Void
affect scope ast = ast

unify scope ast = unify_ scope ast
unify_ scope (List xs) = List $ map (unify scope) xs
unify_ scope (Apply name argv) = dispatch scope name (map (unify scope) argv)
unify_ scope x = x

dispatch scope_ name argv = go name argv
  where
    scope = into scope_ name
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
    dispatch_op2 op argv = miss scope $ "Operator not found: " ++ op ++ " " ++ show argv
    dispatch_eq Void Void = True
    dispatch_eq (Bool a) (Bool b) = a == b
    dispatch_eq (Int a) (Int b) = a == b
    dispatch_eq (String a) (String b) = a == b
    dispatch_eq (List a) (List b) = all id (zipWith dispatch_eq a b)
    dispatch_eq (Struct a) (Struct b) = show a == show b
    dispatch_eq a@(Enum _ _) b@(Enum _ _) = show a == show b
    dispatch_eq a@(Throw _) b@(Throw _) = show a == show b
    dispatch_eq a b = miss scope $ "Invalid equal: " ++ show a ++ " == " ++ show b
    dispatch_func _ (a@(Throw _):_) = a
    dispatch_func "trace" args = trace ("TRACE: " ++ show args) Void
    dispatch_func "if"  [Bool a, b, c] = if a then b else c
    dispatch_func "map"  [List xs, ast] = List $ map (\arg -> unify scope $ apply "" scope [arg] ast) xs
    dispatch_func "mapi"  [List xs, ast] = List $ map (\(i, arg) -> unify (push scope "i" (Int i)) $ apply "" (push scope "i" (Int i)) [arg] ast) (zip [0..] xs)
    dispatch_func "find"  [List xs, ast] = case (filter (\x -> (apply "__find__" scope [x] ast) == Bool True) xs) of
      [] -> miss scope $ "Finding element not found " ++ show ast ++ "\n  in " ++ show xs
      (x:_) -> x
    dispatch_func "has"  [List xs, ast] = Bool $ elem ast xs
    dispatch_func "has"  [String xs, String x] = Bool $ string_contains xs x
    dispatch_func "join" [List xs, String glue] = String $ string_join glue (map to_string xs)
    dispatch_func "sub"  [String a, String b, String c] = String $ string_replace (length a) a b c
    dispatch_func "rsub1"  [String a, String b, String c] = String $ reverse $ string_replace 1 (reverse a) (reverse b) (reverse c)
    dispatch_func "to_int" [String s] = Int (read s :: Int)
    dispatch_func "to_float" [String s] = Float (read s :: Double)
    dispatch_func "to_string" [Int n] = String $ show n
    dispatch_func "to_string" [Float n] = String $ show n
    dispatch_func "to_string" [String s] = String $ '"' :  s ++ "\""
    dispatch_func "not" [Bool b] = Bool $ not b
    dispatch_func "length" [List s] = Int $ length s
    dispatch_func "length" [String s] = Int $ length s
    dispatch_func "slice"  [String s, Int n] = String $ drop n s
    dispatch_func "slice"  [String s, Int n, Int m] = String $ take m (drop n s)
    dispatch_func "at" [String s, Int index] = if index < length s
      then String $ [(s !! index)]
      else Throw $ "out of index " ++ show index ++ " in \"" ++ s ++ "\""
    dispatch_func "at" [List s, Int index] = if index < length s
      then s !! index
      else Throw $ "out of index " ++ show index ++ " in \"" ++ show s ++ "\""
    dispatch_func name argv = miss scope $ "Undefined function: " ++ name ++ " " ++ show argv
    dispatch_call name ((Flow props throws fields):argv) = go
      where
        local = pushs scope $ (zip props argv) ++ throws ++ fields
        go = if elem name (map fst throws) then go_throw else go_method
        go_throw = find name (Scope [] throws [])
        go_method = if (length props) <= (length argv)
          then case find name local of
            Func args body -> if (length args) == (length argv) - (length props)
              then affect (pushs local $ (zip args (drop (length props) argv))) body
              else miss scope $ "Too many arguments " ++ name ++ " have " ++ show props ++ " but args " ++ show argv
            x -> if (length props) == (length argv)
              then affect local x
              else miss scope $ "Too many arguments '" ++ name ++ "' have " ++ show props ++ " but args " ++ show argv
          else miss scope $ "Few arguments " ++ name ++ " have " ++ show props ++ " but args " ++ show argv
    dispatch_call name (s@(Struct fields):argv) = case lookup name fields of
      Just body -> apply name (pushs scope fields) argv body
      _ -> apply name scope (s : argv) $ find name scope
    dispatch_call name argv = apply name scope argv $ find name scope

block :: Scope -> Env -> [AST] -> AST -> (AST, Env)
block scope local _ ast@(Throw _) = (ast, local)
block scope local [] ast = (ast, local)
block scope local (head_ast:rest) last = branch head_ast rest
  where
    uni :: AST -> AST
    uni ast = unify (pushs scope local) ast
    branch :: AST -> [AST] -> (AST, Env)
    branch ast rest = case uni ast of
      a@(Throw _) -> (a, local)
      Block [Apply "|" [l, r]] -> case branch l [] of
        (Throw _, _) -> branch r rest
        _ -> branch l rest
      Block steps -> let (ast, local2) = block (pushs scope local) [] steps Void in block scope (local2 ++ local) rest ast
      Define name exp -> block (push scope name (uni exp)) local rest Void
      Assign name exp -> let (ast, local2) = branch exp [] in block (push scope name ast) (local2 ++ local) rest ast
      Update name exp -> let (ast, local2) = branch exp [] in block scope ((name, ast) : local2 ++ local) rest ast
      ast -> block scope local rest ast

find name scope = case lookup name (whole scope) of
  Just (Apply name' []) -> if name == name'
    then miss scope $ "Circle reference " ++ name ++ " in " ++ (show $ map fst (whole scope))
    else find name' scope
  --Just v@(Throw x) -> trace ("throw: " ++ name ++ show env) $ v
  Just v -> v
  _ -> miss scope $ "Not found '" ++ name ++ "' in " ++
        "\n   local: " ++ (show $ map fst (local scope)) ++
        "\n  global: " ++ (show $ map fst (global scope))

apply name scope argv ast = go (unify scope ast)
  where
    go (Func args body)
      | length argv == 0 && length args == 0 = body
      | length argv == 0 = Func args body
      | length argv == length args = run args body
      | otherwise = miss scope $ "Miss match " ++ name ++ " " ++ (show $ length args) ++ " != " ++ (show $ length argv) ++
        " in " ++ show args ++ " != " ++ show argv ++ " => " ++ show body
    go v = if length argv == 0 then v else miss scope $ "Can't apply: " ++ name ++ " " ++ show v ++ "\nwith " ++ show argv
    run args Void = Struct $ zip args argv
    run args (Enum name Void) = Enum name (Struct $ zip args argv)
    run args (Struct kvs) = Struct $ (zip args argv) ++ kvs
    run args (Match matches) = match args matches
    run args (Block steps) = Block $ (map (\(k, v) -> Define k v) (zip args argv)) ++ steps
    run args body = case unify (pushs scope (zip args argv)) body of
      Block steps -> Block $ (zipWith Define args argv) ++ steps
      Func args2 body2 -> Func args2 (Apply "_apply" [Struct (("_apply", body2) : (zip args argv))])
      result -> result
    match :: [String] -> [([AST], AST)] -> AST
    match args all_conds = if (length argv) == (length args)
      then match_ args all_conds
      else miss scope $ "Unmatch " ++ name ++ " " ++ show args ++ " != " ++ show argv
      where
        match_ args [] = miss scope $ "Miss match\ntarget: " ++ show argv ++ "\ncase: " ++ string_join "\ncase: " (map (show . fst) all_conds)
        match_ args ((conds,branch):rest) = if (length argv) == (length conds)
          then if all id (zipWith is argv (map (unify scope) conds))
            then unify (pushs scope (zip args (map unwrap argv))) branch
            else match_ args rest
          else miss scope $ "Unmatch " ++ name ++ " " ++ show args ++ " where " ++ show argv ++ " != " ++ show conds ++ " => " ++ show branch
    unwrap (Enum _ v) = v
    unwrap v = v
    is _ Void = True
    is (Enum t1 _) (Enum t2 _) = t1 == t2
    is v1 v2 = v1 == v2

-- utility
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
trace_ mark x = trace ("* " ++ (show mark) ++ " " ++ show x) x
