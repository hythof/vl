module Eval (eval) where

import Define

eval :: [(String, AST)] -> AST -> AST
eval s (Op2 "+" (Int a) (Int b)) = Int $ a + b
eval s (Op2 "-" (Int a) (Int b)) = Int $ a - b
eval s (Op2 "*" (Int a) (Int b)) = Int $ a * b
eval s (Op2 "/" (Int a) (Int b)) = Int $ a `div` b
eval s (Op2 "+" (Float a) (Float b)) = Float $ a + b
eval s (Op2 "-" (Float a) (Float b)) = Float $ a - b
eval s (Op2 "*" (Float a) (Float b)) = Float $ a * b
eval s (Op2 "/" (Float a) (Float b)) = Float $ a / b
eval s (Op2 op a b) = eval s $ Op2 op (eval s a) (eval s b)

eval s (Func [] ast) = eval s ast

eval s (If cond a b) = case eval s cond of
    (Bool True) -> eval s a
    (Bool False) -> eval s b

eval s (Ref name) = eval s $ find name s

eval s (Apply name args) = case find name s of
    (Func binds ast) -> apply binds values ast
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
            _ -> ast

split :: String -> [String] -> [String]
split [] acc = reverse acc
split ('.':xs) acc = split xs ([[]] ++ acc)
split (x:xs) [] = split xs [[x]]
split (x:xs) (y:ys) = split xs ((y ++ [x]) : ys)
