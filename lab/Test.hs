module Lab where

import Debug.Trace (trace)
import AST
import Parser (parse)
import Evaluator (eval)

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
    , ("e", "\"hello\".1")
    , ("throw:out of index a.1", "\"a\".1")
    , ("z", "\"hello\".9 | \"z\"")
    , ("5", "\"hello\".length")
    , ("llo", "\"hello\".slice(2)")
    , ("ll", "\"hello\".slice(2 2)")
    , ("3", "1 + 2")
    , ("-1", "1 - 2")
    , ("6", "2 * 3")
    , ("2", "(x => x + 1)(1)")
    , ("3", "\n  a = 1\n  b = a + 1\n  a + b")
    , ("1", "\n  return 1\n  2")
    , ("throw:cancel", "\n  throw cancel\n  2")
    ]
  test struct_code [
      ("2", "either(1 2).right")
    , ("1", "either(1 2).left")
    ]
  test enum_code [
      ("just:value:1", "maybe.just(1)")
    , ("none", "maybe.none")
    , ("value:1", "to_int(maybe.just(1))")
    ]
  test flow_code [
      ("hello", "parser(\"hello\").input")
    , ("h", "parser(\"hello\").satisfy(x => x == \"h\")")
    , ("throw:miss", "parser(\"Hello\").satisfy(x => x == \"h\")")
    , ("throw:eof", "parser(\"\").satisfy(x => x == \"h\")")
    , ("throw:eof", "parser(\"\").eof")
    , ("throw:miss", "parser(\"\").miss")
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
  , "to_int m ="
  , "| maybe.just = m"
  , "| maybe.none = _"
  ]
flow_code = unlines [
    "flow parser a:"
  , "  eof"
  , "  miss reason string"
  , "  input string"
  , "  satisfy f ="
  , "    c = input.0 | eof"
  , "    f(c) || miss"
  , "    input := input.slice(1)"
  , "    c"
  ]
match_code = unlines [
    "match n ="
  , "| true = 0"
  , "| 0 = n + 1"
  , "| \"hello\" = 2"
  , "| _ = n"
  ]

test src tests =  mapM_ (runTest src) tests
runTest src (expect, exp) = runAssert expect (src ++ "\nmain = " ++ exp) exp
runAssert expect src exp = if expect == result
  then putStr "."
  else error $ makeMessage (expect ++ " != " ++ result ++ "\n" ++ exp)
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

fmt (Void) = "()"
fmt (Bool True) = "true"
fmt (Bool False) = "false"
fmt (Int n) = show n
fmt (String s) = s
fmt (List xs) = "[" ++ (map_reduce fmt xs) ++ "]"
fmt (Ref s) = s
fmt (Op2 o l r) = (fmt l) ++ " " ++ o ++ " " ++ (fmt r)
fmt (Apply o args) = (fmt o) ++ "(" ++ (map_reduce fmt args) ++ ")"
fmt (Method o m args) = (fmt o) ++ "." ++ m ++ "(" ++ (map_reduce fmt args) ++ ")"
fmt (Struct env) = (map_reduce (\(k,v) -> k ++ ":" ++ (fmt v)) env)
fmt (Enum name Void) = name
fmt (Enum name v) = name ++ ":" ++ fmt v
fmt (Func args body) = (map_reduce id args) ++ "=>" ++ fmt body
fmt (Assign name ast) = name ++ " := " ++ fmt ast
fmt (Return ast) = "return:" ++ fmt ast
fmt (Throw ast) = "throw:" ++ ast
fmt ast = show ast
map_reduce f xs = go xs
  where
    go [] = ""
    go [x] = f x
    go (x:xs) = (f x) ++ " " ++ (go xs)
