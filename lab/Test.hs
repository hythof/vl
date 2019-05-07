module Lab where

import Debug.Trace (trace)
import AST
import Parser (parse)
import Evaluator (eval, to_string)

main = do
  test match_code [
      ("0", "match(true)")
    , ("1", "match(0)")
    , ("2", "match(\"hello\")")
    , ("3", "match(3)")
    ]
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
    , ("e", "\"hello\".at(1)")
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
      ("h", "parser(\"hello\").satisfy(x => x == \"h\")")
    , ("01", "parser(\"01\").zero_one")
    , ("throw:miss", "parser(\"Hello\").satisfy(x => x == \"h\")")
    , ("throw:eof", "parser(\"\").satisfy(x => x == \"h\")")
    , ("throw:eof", "parser(\"\").eof")
    , ("throw:miss", "parser(\"\").miss")
    ]
  test vl_code [
      ("1", "parser(\"1\").read_one([\"1\"])")
    , ("int(value:1)", "parser(\"1\").parse_int")
    , ("int(value:123)", "parser(\"123\").parse_int")
    , ("op2(op:+\nleft:int(value:1)\nright:int(value:2))", "parser(\"1+2\").parse_op2")
    ]
  putStrLn "ok"

struct_code = unlines [
    "struct either:"
  , "  left int"
  , "  right int"
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
  ]
vl_code = unlines [
    "enum ast:"
  , "  int value int"
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
  , "    op <- read_one([\"+\"])"
  , "    right <- parse_op2"
  , "    ast.op2(op left right)"
  , "  parse_int = "
  , "    v <- read_one([\"0\" \"1\" \"2\" \"3\"]).many1.fmap(s => s.join(\"\").to_int)"
  , "    ast.int(v)"
  , "  read_one candidates = satisfy(x => candidates.has(x))"
  , "  many1 p1 ="
  , "    x <- p1"
  , "    xs <- p1.many"
  , "    [x] ++ xs"
  , "  many p = p.many_acc([])"
  , "  many_acc pa acc = pa.fmap(a => pa.many_acc(acc ++ [a])) | acc"
  , "  fmap pf ff ="
  , "    v <- pf"
  , "    ff(v)"
  ]

test src tests =  mapM_ (runTest src) tests
runTest src (expect, exp) = runAssert expect (src ++ "\nmain = " ++ exp) exp
runAssert expect src exp = if expect == result
  then putStr "."
  else error $ makeMessage ("`" ++ expect ++ " != " ++ result ++ "`\nexp: " ++ exp)
  where
    env = get_env src
    result = case lookup "main" env of
      Just v -> fmt $ eval env v
      _ -> error $ makeMessage "Not found main"
    get_env src = case parse src of
      (env, "") -> env
      (env, rest) -> error $ makeMessage ("Failed parsing: `" ++ rest ++ "`")
    makeMessage message = message ++
      concat (map (\(k, v) -> "\n- " ++ k ++ "\t" ++ show v) env) ++
      "\n" ++ src ++ "\n"

fmt (String s) = s
fmt ast = to_string ast
