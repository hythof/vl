module Eval (eval) where

import Define
import Debug.Trace

eval :: [(String, AST)] -> AST -> AST
eval s (Op2 op a b) = op2 op (eval s a) (eval s b)

eval s (Func [] ast) = eval s ast

eval s (New name []) = Instance name []

eval s (Ref name) = eval s $ find name s

eval s (If cond a b) = case eval s cond of
    (Bool True) -> eval s a
    (Bool False) -> eval s b

eval s (Case _ [] other) = eval s other
eval s (Case x ((y, current):xs) other) = check (eval s x) (eval s y)
  where
    check (Instance name2 values) (Match name1 args) = if name1 == name2 then
            eval (s ++ (zip args values)) current
        else
            eval s $ Case x xs other
    check a b = if show a == show b then
            eval s current
        else
            eval s $ Case x xs other

eval s (Apply name args) = case find name s of
    (Func binds ast) -> apply binds values ast
    (New klass fields) -> Instance klass values
    (Struct fields) -> Struct $ zipWith (\(x, _) y -> (x, y)) fields values
    ast -> eval s ast
  where
    apply :: [String] -> [AST] -> AST -> AST
    apply needs seeds ast = case length needs `compare` length seeds of
      LT -> Error ("overflow arguments " ++ (show name))
      EQ -> eval (s ++ (zip needs seeds)) ast
      GT -> Closure (s ++ (zip needs seeds)) ast
    values = map (\x -> eval s x) args

eval s ast = ast

-- private
op2 "==" a b = Bool $ (show a) == (show b)
op2 op (Int a) (Int b) = case op of
    "+" -> Int $ a + b
    "-" -> Int $ a - b
    "*" -> Int $ a * b
    "/" -> Int $ a `div` b
    _ -> error $ "unknown int operator " ++ show op
op2 op (Float a) (Float b) = case op of
    "+" -> Float $ a + b
    "-" -> Float $ a - b
    "*" -> Float $ a * b
    "/" -> Float $ a / b
    _ -> error $ "unknown float operator " ++ show op

find name s = f (split name []) s
  where
    f _ [] = Error $ "not found " ++ (show name) ++ "\n  in " ++ (show $ map (\(x, _) -> x) s)
    f [] y = Error $ show y ++ show (split name []) ++ name
    f [x] ((define, ast):ys) = if x == define
        then ast
        else f [x] ys
    f (x:xs) ((define, ast):ys) = if x /= define
        then f (x:xs) ys
        else case ast of
            (Struct zs) -> f xs zs
            (Class _ zs) -> f xs $ map (\z -> (head z, New (x ++ "." ++ head z) (tail z))) zs
            _ -> ast

split :: String -> [String] -> [String]
split [] acc = reverse acc
split ('.':xs) acc = split xs ([[]] ++ acc)
split (x:xs) [] = split xs [[x]]
split (x:xs) (y:ys) = split xs ((y ++ [x]) : ys)
