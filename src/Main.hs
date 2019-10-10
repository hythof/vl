module Main where

import Base
import Parser (parse)
import Compiler (compile)
import System.Process (system)

main = do
  test "1" "1"
  test "12" "12"
  test "5" "2+3"
  test "-1" "2-3"
  test "6" "2*3"
  test "0" "2/3"
  test "2" "2%3"
  test "9" "2+3+4"
  test "14" "2+(3*4)"
  test "20" "(2+3)*4"
  test "1" "x=1\nx"
  test "5" "x=2\ny=3\nx+y"
  test "-1" "x=2\ny=3\nx-y"
  test "6" "x=2\ny=3\nx*y"
  test "0" "x=2\ny=3\nx/y"
  test "2" "x=2\ny=3\nx%y"
  test "3" "x=2\nx:=3\nx"
  test "3" "x=2\nx:=3\ny=4\nx"
  test "5" "x=2\nx+=3\nx"
  test "-1" "x=2\nx-=3\nx"
  test "6" "x=2\nx*=3\nx"
  test "0" "x=2\nx/=3\nx"
  test "2" "x=2\nx%=3\nx"
  test "1" "x={1}\nx"
  test "3" "x={y=1\ny+=2\ny}\nx"
  test "1" "id x = x\nid(1)"
  test "5" "add a b = a + b\nadd(2 3)"
  test "9" "add a b c = a + b + c\nadd(2 3 4)"
  test "true" "true"
  test "false" "false"
  test "true" "true && true"
  test "false" "false && true"
  test "true" "true || true"
  test "true" "false || true"
  test "true" "1 == 1"
  test "false" "1 == 2"
  test "false" "1 != 1"
  test "true" "1 != 2"
  test "true" "1 >= 1"
  test "false" "1 > 1"
  test "true" "1 <= 1"
  test "false" "1 < 1"
  test "1" "if(true 1 2)"
  test "2" "if(false 1 2)"
  test "1" "x=0\nif(true 1 (2/x))"
  test "1" "x=0\nif(false (2/x) 1)"
  test "a" "\"a\""
  test "hi" "\"hi\""
  test "a1" "x=\"a1\"\nx"
  test "h" "\"hi\".nth(0)"
  test "h" "x=\"hi\"\nx.nth(0)"
  test "i" "x=\"hi\"\nx.nth(1)"
  test "1" "\"h\".length"
  test "1" "x=\"h\"\nx.length"
  test "2" "x=\"hi\"\nx.length"
  test "1" "x=1\ny=x\ny"
  test "h" "x=\"h\"\ny = x.append(\"i\")\nx"
  test "hi" "x=\"h\"\ny = x.append(\"i\")\ny"
  test "hi" "x=\"h\"\ny = \"i\"\nx.append(y)"
  test "h" "\"hi\".nth(0)"
  test "h" "\"hi\".prefix(1)"
  test "hi" "\"hi\".prefix(2)"
  test "hi" "\"hi\".prefix(3)"
  test "h" "\"hi\".slice(0 1)"
  test "i" "\"hi\".slice(1 1)"
  test "hi" "\"hi\".slice(0 2)"
  test "hi" "\"hi\".slice(0 3)"
  test "void()" "void {}\nvoid"
  putStrLn "done"

eval :: [AST] -> IO String
eval x = do
  system $ "mkdir -p /tmp/llvm"
  let ll_main = compile x
  ll_lib <- readFile "./lib.ll"
  let ll = ll_lib ++ ll_main
  writeFile "/tmp/llvm/v.ll" ll
  system $ "(cd /tmp/llvm && lli v.ll > stdout.txt)"
  readFile "/tmp/llvm/stdout.txt"

test :: String -> String -> IO ()
test expect input = go
  where
    go = do
      case parse input of
        Nothing -> error $ "Parser error " ++ input ++ " expect: " ++ expect
        Just (x, s) -> if pos s == len s
          then run x
          else error $ "Parser error\n- remaining: " ++ (drop (pos s) (src s)) ++ "\n- src: " ++ input
    run ast = do
      stdout <- eval ast
      if expect == stdout
      then putChar '.'
      else putStrLn $ "x\n- expect: " ++ show expect ++ "\n-   fact: " ++ show stdout ++ " :: " ++ show ast
