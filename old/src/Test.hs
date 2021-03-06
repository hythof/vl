module Test where

import Common

main = do
  test flow_code [
    --("", "parser(\"val\").string(\"\")")
    --("v", "parser(\"val\").string(\"v\")")
    ("vv", "parser(\"val\").string(\"vv\")")
    --, ("parser.miss", "parser(\"val\").string(\"z\")")
    ]
a = do
  test "" [
      ("2", "(\"h\" . \"i\").length")
    , (" ", "\" \"")
    , ("1\\n", "\"1\n\"")
    , ("2\\n", "\"2\\n\"")
    , ("3", "1 + \n2")
    , ("3", "(a => a + 1)(2)")
    , ("hi", "\"h\" . \n  \"i\"")
    , ("[1 2]", "[1] ++ [2]")
    ]
  test values_code [
      ("s", "s")
    , ("1", "i")
    , ("-1", "in")
    , ("1.0", "r")
    , ("-1.0", "rn")
    , ("true", "bt")
    , ("false", "bf")
    , ("[]", "l0")
    , ("[s 1 1.0 true false 2]", "l7")
    , ("3", "ref")
    , ("multiple\\nlines", "sq")
    , ("5", "\"hello\".length")
    ]
  test enum_code [
      ("ast.int(value:1)", "ast.int(1)")
    , ("ast.none", "ast.none")
    , ("ast.func(args:[]  body:ast.str(value:hello))", "ast.func([] ast.str(\"hello\"))")
    , ("1", "ast.int(1).value")
    ]
  test struct_code [
      ("age", "attribute(\"age\" 35).key")
    , ("35", "attribute(\"age\" 35).val")
    ]
  test match_code [
      ("zero", "m(0)")
    , ("one", "m(1)")
    , ("many", "m(2)")
    , ("many", "m(1.0)")
    , ("true", "m(true)")
    , ("str", "m(\"s\")")
    , ("3", "m(1).length")
    , ("4", "m(0).length")
    ]
  test enum_match_code [
      ("1", "m(ast.int(1))")
    , ("none", "m(ast.none)")
    ]
  test stmt_code [
      ("6", "stmt(1 2)")
    , ("3", "update(1)")
    , ("9", "update(5)")
    , ("99", "assign(0)")
    , ("1", "local_func(1)")
    ]
  test flow_code [
      ("true", "t(1)")
    , ("false", "f(1)")
    , ("val", "parser(\"val\").src")
    , ("v", "parser(\"val\").satisfy(t)")
    , ("va", "parser(\"val\").string(\"va\")")
    , ("val", "parser(\"val\").string(\"val\")")
    , ("parser.miss", "parser(\"val\").string(\"z\")")
    , ("parser.eof", "parser(\"\").satisfy(f)")
    , ("parser.miss", "parser(\"val\").satisfy(f)")
    ]
  test primitive_code [
      ("true", "is_l(\"l\")")
    , ("[2 3 4]", "ai3.map(inc)")
    , ("6", "ai3.fold(0 add)")
    , ("h,e,l,l,o", "as3.join(\",\")")
    , ("[l l]", "as3.filter(is_l)")
    , ("[l l]", "as3.filter(\nis_l\n)")
    , ("3", "add.bind(1)(2)")
    , ("3", "add(\n1\n  2\n)")
    , ("3", "id(x => x)(3)")
    ]
  putStrLn "ok"

values_code = unlines [
    "s = \"s\""
  , "i = 1"
  , "in = -1"
  , "r = 1.0"
  , "rn = -1.0"
  , "bt = true"
  , "bf = false"
  , "l0 = []"
  , "l7 = [s i\nr bt bf add(i i)]"
  , "add x y = x + y"
  , "ref = add(1 2)"
  , "sq = `"
  , "multiple"
  , "lines"
  , "`"
  ]
struct_code = unlines [
    "struct attribute:"
  , "  key str"
  , "  val int"
  ]
enum_code = unlines [
    "enum ast a:"
  , "  int:"
  , "    value a"
  , "  str:"
  , "    value str"
  , "  func:"
  , "    args [str]"
  , "    body ast"
  , "  none"
  ]
match_code = unlines [
    "m ="
  , "| 0 = \"zero\""
  , "| 1 = \"one\""
  , "| true = \"true\""
  , "| string = \"str\""
  , "| _ = \"ma\" .\n  \"ny\""
  ]
enum_match_code = enum_code ++ (unlines [
    "m e ="
  , "| ast.int = e.value"
  , "| ast.none = \"none\""
  ])
stmt_code = unlines [
    "stmt a b ="
  , "  x = a + b"
  , "  z = add(add(a b) x)"
  , "  z"
  , "add x y = x + y"
  , "update a ="
  , "  a += 2"
  , "  a -= 1"
  , "  a *= 3"
  , "  a /= 2"
  , "  a"
  , "assign a ="
  , "  a := 99"
  , "  a"
  , "oneline a = a := 88; a += 12; a"
  , "local_func x ="
  , "  add a b = a + b"
  , "  flip y ="
  , "  | 0 = 1"
  , "  | 1 = 0"
  , "  add(x flip(x))"
  ]
flow_code = unlines [
    "flow parser:"
  , "  eof"
  , "  miss:"
  , "    reason string"
  , "  src string"
  , "step parser:"
  , "  satisfy f ="
  , "    c = src.0 .or eof"
  , "    f(c) || miss(\"miss match\")"
  , "    src := src.slice(1)"
  , "    c"
  , "  string s = _string(s); s"
  , "  _string s ="
  , "  | \"\" = \"\""
  , "  | _ = satisfy(x => x == s.0); _string(s.slice(1))"
  , "t _ = true"
  , "f _ = false"
  ]
primitive_code = unlines [
    "a0 = []"
  , "ai3 = [1 2 3]"
  , "as3 = [\"h\" \"e\" \"l\" \"l\" \"o\"]"
  , "inc x = x + 1"
  , "add x y = x + y"
  , "is_l s = s == \"l\""
  , "id x = x"
  ]
test _ [] = return ()
test common ((expect, src):rest) = do
  run_test expect $ "main = " ++ src ++ "\n" ++ common
  test common rest
run_test expect src = case parse src of
  Left (msg, env) -> failed_parsing msg env
  Right scope -> case evaluate scope (snd $ scope !! 0) of
    Success a scope -> if expect == fmt a
      then putStr "."
      else test_failed a scope
    Fail msg scope -> failed_evaluation msg scope
 where
  test_failed actual scope = do
    putStrLn ""
    putStrLn $ "expect: " ++ expect
    putStrLn $ "actual: " ++ fmt actual
    putStrLn $ "   ast: " ++ show actual
    putStrLn $ fmt_scope scope
    fail $ "failed test"
  failed_parsing reason env = do
    putStrLn ""
    putStrLn $ "parse: " ++ reason
    putStrLn $ fmt_env env
    putStrLn $ "   src: " ++ src
    fail $ "failed parsing"
  failed_evaluation reason scope = do
    putStrLn ""
    putStrLn $ "eval: " ++ reason
    putStrLn $ fmt_scope scope
    putStrLn $ "   src: " ++ src
    fail $ "failed execution"
