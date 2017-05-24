module Main where
import           AST
import           Eval
import           Parser

check :: AST -> AST -> Either (AST, AST) (AST, AST)
check expect fact = if expect == fact then Right (expect, fact) else Left (expect, fact)

ok :: AST -> String -> Either (AST, AST) (AST, AST)
ok expect source = case parse top source of
    Left x -> Left (expect, Error x)
    Right x -> check expect $ eval [] x

def :: AST -> String -> Either (AST, AST) (AST, AST)
def expect source = case parse file example of
    Left x -> Left (expect, Error x)
    Right xs -> case parse top source of
        Left x -> Left (expect, Error x)
        Right x -> check expect (eval xs $ x)

display :: [Either (AST, AST) (AST, AST)] -> IO ()
display xs = do
    putStrLn $ map line xs
    mapM_ detail xs
  where
    line (Left _) = 'F'
    line (Right _) = '.'
    detail (Left x) = print x
    detail (Right _) = return ()

main = do
    display [
        ok (Int 3) "1 + 2"
      , ok (Int (-1)) "1 - 2"
      , ok (Int 6) "2 * 3"
      , ok (Int 2) "4 // 2"
      , ok (Int 1) "5 % 2"
      , ok (Int 9) "3 ** 2"
      , ok (Int 4) "1 << 2"
      , ok (Int 2) "8 >> 2"
      , ok (Int 3) "1 | 2"
      , ok (Int 3) "3 & 7"
      , ok (Real 3.0) "1.0 + 2.0"
      , ok (Real (-1.0)) "1.0 - 2.0"
      , ok (Real 6.0) "2.0 * 3.0"
      , ok (Real 2.0) "4.0 / 2.0"
      , ok (Real 9.0) "3.0 ** 2.0"
      , ok (String "hello world") "\"hello \" + \"world\""
      , ok (List [Int 1, Int 2]) "[1] + [2]"
      , ok (List [Int 3]) "[1 + 2]"
      , ok (Bool True) "T & T"
      , ok (Bool False) "T & F"
      , ok (Bool False) "F & T"
      , ok (Bool False) "F & F"
      , ok (Bool True) "T | T"
      , ok (Bool True) "T | F"
      , ok (Bool True) "F | T"
      , ok (Bool False) "F | F"

      , ok (Int 1) "if T 1 2"
      , ok (Int 2) "if F 1 2"
      , ok (Int 2) "if (1 == 1) 2 3"
      , ok (Int 4) "if (1 == 2) 3 4"

      , def (Int 3) "a + b"
      , def (Int 1) "top.i"
      , def (Int 3) "top.i + top.nest.j"
      , def (Int 2) "top.nest.j"
      , def (Int 3) "top.nest.ij"
      , def (Int 5) "(add 2 3)"

      , def (Int 3) "add a b"
      , def (Bool False) "a == b"
      , def (Int 1) "id a"
      , def (Int 2) "(id b)"
      , def (Int 3) "add (id a) (id b)"
      , def (Int 4) "add (id a) (add (id a) (id b))"
      , def (List [Int 4]) "[add (id a) (add (id a) (id b))]"
      , def (Struct [("x", Int 1), ("y", Int 2)]) "move"
      , def (Int 1) "move.x"
      , def (Int 2) "move.y"
      , def (Struct [("x", Int 0), ("y", Int 0)]) "point"
      , def (Struct [("x", Int 0), ("y", Int 0), ("z", Int 0)]) "transform"
      , def (Int 0) "transform.x"
      , def (Int 0) "transform.y"
      , def (Int 0) "transform.z"
      , def (Int 1) "set.x"
      , def (Int 2) "set.y"
      , def (Int 3) "set.z"
      , def (Tag "zero" [] []) "zero"
      , def (Tag "vector1" ["int"] []) "vector1"
      , def (Tag "vector1" ["int"] [Int 1]) "vector1 1"
      , def (Tag "vector2" ["int", "int"] []) "vector2"
      , def (Tag "vector2" ["int", "int"] [Int 1, Int 2]) "vector2 1 2"
      , def (Tag "true" [] []) "true"
      , def (Tag "false" [] []) "false"
      ]

example = "a 1\n" ++
    "b 2\n" ++
    "id x = x\n" ++
    "add a b = a + b\n" ++
    "point = {x 0, y 0}\n" ++
    "order = {z 0}\n" ++
    "move = (point 1 2)\n" ++
    "transform = point + order\n" ++
    "set = (transform 1 2 3)\n" ++
    "zero\n" ++
    "vector1 int\n" ++
    "vector2 int int\n" ++
    "true | false\n" ++
    "top = {i 1, nest {j 2, ij = i + j}}"
