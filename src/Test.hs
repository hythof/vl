module Test where

import Debug.Trace (trace)
import Common

main = do
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
    , ("many", "m(true)")
    , ("many", "m(\"s\")")
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
    ]
  test state_code [
      ("val", "parser(\"val\").src")
    , ("true", "t(1)")
    , ("false", "f(1)")
    , ("v", "parser(\"val\").satisfy(t)")
    , ("parser.miss_error", "parser(\"val\").satisfy(f)")
    ]
  putStrLn "ok"
 where
  values_code = unlines [
      "s = \"s\""
    , "i = 1"
    , "in = -1"
    , "r = 1.0"
    , "rn = -1.0"
    , "bt = true"
    , "bf = false"
    , "l0 = []"
    , "l7 = [s i r bt bf add(i i)]"
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
    , "| _ = \"many\""
    ]
  enum_match_code = enum_code ++ (unlines [
      "m e ="
    , "| ast.int = e.value"
    , "| ast.none = \"none\""
    ])
  stmt_code = unlines [
      "stmt a b ="
    , "  v = a + b"
    , "  z = add(add(a b) v)"
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
    ]
  state_code = unlines [
      "state parser a:"
    , "  src str"
    , "  miss"
    , "  satisfy f ="
    , "    c = src.0"
    , "    f(c) || miss"
    , "    c"
    , "t _ ="
    , "| _ = true"
    , "f _ ="
    , "| _ = false"
    ]
  test _ [] = return ()
  test common ((expect, src):rest) = do
    run_test expect $ "main = " ++ src ++ "\n" ++ common
    test common rest
  run_test expect src = if expect == act
    then putStr "."
    else do
      putStrLn ""
      putStrLn $ "expect: " ++ expect
      putStrLn $ "actual: " ++ act
      putStrLn $ "   ast: " ++ show ret
      dump src
      fail $ "failed test"
   where
    env = parse src
    ast = snd $ env !! 0
    ret = eval env ast
    act = (fmt ret) ++ err
    err = case lookup "err" env of
      Just e  -> fmt e
      Nothing -> ""
