module Main where

import Debug.Trace (trace)
import AST
import Parser (parse)
import Evaluator (eval, to_string)
import Control.Monad (when)
import System.Process (runCommand, waitForProcess)

main = tests
_tests = do
  testC [
      ("[]", unlines ["[]"])
    , ("[1]", unlines ["[1]"])
    , ("[1 2 3]", unlines ["[1 2 3]"])
    ]
  putStrLn "done of tmp test"
tests = do
  test "id a = a" [
      ("[]", "[]")
    , ("[1]", "[1]")
    , ("[1]", "[\n1\n]")
    , ("[1 2]", "[\n1\nid(2)\n]")
    , ("1", "id(1)")
    , ("1", "id(\n1\n)")
    , ("true", "true && true")
    , ("true", "false || true")
    , ("true", "1 == 1")
    , ("false", "1 == 2")
    , ("false", "1 > 2")
    , ("true", "1 < 2")
    , ("true", "2 >= 2")
    , ("true", "2 <= 2")
    , ("hello", "\"h\" . \"ello\"")
    , ("5", "(\"h\" . \"ello\").length")
    , ("e", "\"hello\".at(1)")
    , ("hello\nworld", "`\nhello\nworld\n`")
    , ("throw:out of index 1 in \"a\"", "\"a\".at(1)")
    , ("z", "\"hello\".at(9) | \"z\"")
    , ("5", "\"hello\".length")
    , ("hello", "\"hello\".slice(0)")
    , ("ello", "\"hello\".slice(1)")
    , ("el", "\"hello\".slice(1 2)")
    , ("3", "1 + 2")
    , ("-1", "1 - 2")
    , ("6", "2 * 3")
    --, ("2", "(x => x + 1)(1)")
    , ("3", "\n  a = 1\n  b = a + 1\n  a + b")
    ]
  test match_code [
      ("0", "match(true)")
    , ("1", "match(0)")
    , ("2", "match(\"hello\")")
    , ("3", "match(3)")
    , ("3", "match2(1 2)")
    , ("99", "match2(9 9)")
    ]
  test struct_code [
      ("2", "either(1 2).right")
    , ("1", "either(1 2).left")
    ]
  test enum_code [
      ("just(value:1)", "maybe.just(1)")
    , ("none", "maybe.none")
    , ("value:1", "to_integer(maybe.just(1))")
    ]
  test flow_code [
      ("h", "parser.satisfy(\"hello\" x => x == \"h\")")
    , ("01", "parser.zero_one(\"01\")")
    , ("throw:miss", "parser.satisfy(\"Hello\" x => x == \"h\")")
    , ("throw:eof", "parser.satisfy(\"\" x => x == \"h\")")
    , ("throw:eof", "parser.eof")
    , ("throw:miss", "parser.miss")
    ]
  test vl_code [
      ("1", "parser.read_one(\"1\" [\"1\"])")
    , ("int(value:1)", "parser.parse_int(\"1\")")
    , ("int(value:123)", "parser.parse_int(\"123\")")
    , ("op2(op:+\nleft:int(value:1)\nright:int(value:2))", "parser.parse_op2(\"1+2\")")
    , ("1", "run(\"1\")")
    , ("3", "run(\"1+2\")")
    , ("-1", "run(\"1-2\")")
    , ("6", "run(\"2*3\")")
    , ("0", "run(\"2/3\")")
    , ("1", "run(\"3/2\")")
    , ("2", "run(\"4/2\")")
    , ("2", "run(\"5/2\")")
    ]
  testC [
      ("1", "1")
    , ("1.1", "1.1")
    , ("5", "2 + 3")
    , ("5.2", "2.1 + 3.1")
    , ("6", "2 * 3")
    , ("6.51", "2.1 * 3.1")
    , ("2", "5 / 2")
    , ("2.5", "5.0 / 2.0")
    , ("hello", "\"hello\"")
    , ("hello", "\"he\" . \"llo\"")
    , ("3", unlines ["a + b", "a = 1", "b = 2"])
    , ("3.2", unlines ["a + b", "a = 1.1", "b = 2.1"])
    , ("2", unlines ["inc(1)", "inc x:i64 = 1 + x"])
    , ("[]", unlines ["[]"])
    , ("[1]", unlines ["[1]"])
    , ("[1 2 3]", unlines ["[1 2 3]"])
    ]
  putStrLn "done"

struct_code = unlines [
    "struct either:"
  , "  left i64"
  , "  right i64"
  ]
enum_code = unlines [
    "enum maybe a:"
  , "  just value a"
  , "  none"
  , "to_integer m ="
  , "| maybe.just = m"
  , "| maybe.none = _"
  ]
flow_code = unlines [
    "flow parser a:"
  , "  eof"
  , "  miss reason string"
  , "  input string"
  , "  satisfy f ="
  , "    c <- input.at(0) | eof"
  , "    f(c) || miss"
  , "    input := input.slice(1)"
  , "    c"
  , "  double p ="
  , "    a <- p"
  , "    b <- p"
  , "    a . b"
  , "  zero_one = satisfy(x => (x == \"0\") || (x == \"1\")).double"
  ]
match_code = unlines [
    "match n ="
  , "| true = 0"
  , "| 0 = n + 1"
  , "| \"hello\" = 2"
  , "| _ = n"
  , "match2 a b ="
  , "| 9 9 = 99"
  , "| _ _ = a + b"
  ]
vl_code = unlines [
    "enum ast:"
  , "  int value i64"
  , "  op2 op string, left ast, right ast"
  , "flow parser a:"
  , "  eof"
  , "  miss reason string"
  , "  input string"
  , "  satisfy f ="
  , "    c <- input.at(0) | eof"
  , "    f(c) || miss"
  , "    input := input.slice(1)"
  , "    c"
  , "  parse_op2 ="
  , "    left <- parse_int"
  , "    parse_op2_remaining(left) | left"
  , "  parse_op2_remaining left ="
  , "    op <- read_one([\"+\" \"-\" \"*\" \"/\"])"
  , "    right <- parse_op2"
  , "    ast.op2(op left right)"
  , "  parse_int = "
  , "    v <- read_one([\"0\" \"1\" \"2\" \"3\" \"4\" \"5\" \"6\" \"7\" \"8\" \"9\"]).many1.fmap(s => s.join(\"\").to_int)"
  , "    ast.int(v)"
  , "  read_one candidates = satisfy(x => candidates.has(x))"
  , "many1 p ="
  , "  x <- p"
  , "  xs <- p.many"
  , "  [x] ++ xs"
  , "many p = p.many_acc([])"
  , "many_acc p acc = p.fmap(a => p.many_acc(acc ++ [a])) | acc"
  , "fmap p ff ="
  , "  v <- p"
  , "  ff(v)"
  , "run code = "
  ,"   v <- parser.parse_op2(code)"
  ,"   eval(v)"
  , "eval v ="
  , "| ast.int = v.value"
  , "| ast.op2 = eval_op2(v.op eval(v.left) eval(v.right))"
  , "eval_op2 op l r ="
  , "| \"+\" _ _ = l + r"
  , "| \"-\" _ _ = l - r"
  , "| \"*\" _ _ = l * r"
  , "| \"/\" _ _ = l / r"
  ]

test src tests =  mapM_ (runTest (get_env src) src) tests
runTest base_env src (expect, exp) = runAssert base_env expect exp
runAssert base_env expect exp = go
  where
    go = if expect == result
      then putStr "."
      else error $ makeMessage env ("`" ++ expect ++ " != " ++ result ++ "`\nexp: " ++ exp)
    src = "main = " ++ exp
    env = base_env ++ (get_env src)
    result = case lookup "main" env of
      Just v -> fmt $ eval env v
      _ -> error $ makeMessage env "Not found main"

testC tests = prepare
  where
    prepare = do
      src <- readFile "c.vl"
      let env = get_env src
      go env tests
    go env [] = return ()
    go env ((expect, src):rest) = do
      runTest env expect src
      go env rest
    runTest env expect src= do
      let go_src = to_go env ("main = " ++ src)
      let go_path = "/tmp/tmp.c"
      let stdout_path = "/tmp/out.txt"
      let cmd = "echo 'COMPILE FAILED' > " ++ stdout_path ++ " && gcc -std=c11 -Wall -O2 " ++ go_path ++ " -o /tmp/a.out && /tmp/a.out > " ++ stdout_path
      writeFile go_path $ go_src ++ "\n"
      pid <- runCommand cmd
      waitForProcess pid
      output <- readFile stdout_path
      if output == expect
        then putStr "."
        else error $ "expect: " ++ expect ++ "\n  fact: " ++ output
    to_go env src = case eval env (Apply "__compile_to_c" [String src]) of
      String s -> s
      ret -> error $ makeMessage env ("invalid the result of compiling: " ++ show ret)

get_env src = case parse src of
  (env, "") -> env
  (env, "\n") -> env
  (env, rest) -> error $ makeMessage env ("Failed parsing: " ++ (unlines $ take 10 $ lines rest) ++ "`")
makeMessage env message = message
  --concat (map (\(k, v) -> "\n- " ++ k ++ "\t" ++ show v) env)
fmt (String s) = s
fmt ast = to_string ast
