module Main where
import           AST
import           Eval
import           Parser

ok :: AST -> String -> String
ok expect source = case parse ast_exp source of
    Left x -> "\n Compile error: " ++ source
    Right x -> if expect == eval env x
      then "."
      else "\nexpect: " ++ (show expect)
        ++ "\n  fact: " ++ (show $ eval env x)
        ++ "\n   ast: " ++ (show x)
        ++ "\nsource: " ++ source
        ++ "\n   env: " ++ (show env)
        ++ "\n"

display :: [String] -> IO ()
display xs = mapM_ putStr xs >> putChar '\n'

env = case parse program code of
  Right v -> v
  Left v -> error v

code = foldr (\x y -> x ++ "\n" ++ y) "" [
    "char 'c'"
  , "int 1"
  , "real 1.1"
  , "list []"
  , "bool false"
  , "string \"\""
  , "add x y = x + y"
  , "struct {}"
  , "algebra a {just a | none}"
  , "call = add 1 2"
  , "seq = (do list)"
  , "assign = (do a <= 7; a)"
  , "bind x y = y" -- dummy
  ]

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
    , ok (Bool True) "true & true"
    , ok (Bool False) "true & false"
    , ok (Bool False) "false & true"
    , ok (Bool False) "false & false"
    , ok (Bool True) "true | true"
    , ok (Bool True) "true | false"
    , ok (Bool True) "false | true"
    , ok (Bool False) "false | false"
    , ok (Int 1) "if true 1 2"
    , ok (Int 2) "if false 1 2"
    , ok (Int 2) "if (1 == 1) 2 3"
    , ok (Int 4) "if (1 == 2) 3 4"
    , ok (Char 'c') "char"
    , ok (Int 1) "int"
    , ok (Real 1.1) "real"
    , ok (List []) "list"
    , ok (Bool False) "bool"
    , ok (String "") "string"
    , ok (Func ["x", "y"] (Call [Ref ["+"], Ref ["x"], Ref ["y"]])) "add"
    , ok (Struct []) "struct"
    , ok (Algebra "just" [String "a"]) "algebra.just"
    , ok (Algebra "just" [Int 1]) "algebra.just 1"
    , ok (Algebra "none" []) "algebra.none"
    , ok (Int 3) "add 1 2"
    , ok (List []) "seq"
    , ok (Int 7) "assign"
    ]
