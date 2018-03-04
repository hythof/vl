module Main where
import           AST
import           Eval
import           Parser

ok :: AST -> String -> String
ok expect source = case parse ast_exp source of
    Left x -> "\n Compile error: " ++ source
    Right x -> if expect == eval [] x
      then "."
      else "\nsource: " ++ source ++
        "\nexpect: " ++ (show expect) ++
        "\n  fact: " ++ (show $ eval [] x) ++
        "\n   ast: " ++ (show x)

display :: [String] -> IO ()
display xs = mapM_ putStr xs

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
      ]
