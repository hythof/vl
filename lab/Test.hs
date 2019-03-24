module Lab where

import Debug.Trace (trace)
import AST
import Parser (parse)
import Evaluator (eval)

main = do
  test "struct either: left int, right int" [
      ("2", "either(1 2).right")
    , ("1", "either(1 2).left")
    ]
  test "enum maybe a: just value a, none" [
      ("just:value:1", "maybe.just(1)")
    , ("none", "maybe.none")
    ]
  test ("flow parser a: input string, eof, miss\n" ++ code_parser_methods) [
      ("true", "parser(true).input")
    , ("throw:eof", "parser(true).eof")
    , ("throw:miss", "parser(true).miss")
    ]
  test "" [
      ("true", "true && true")
    , ("true", "false || true")
    , ("3", "1 + 2")
    , ("-1", "1 - 2")
    , ("6", "2 * 3")
    , ("3", "\n  a = 1\n  b = a + 1\n  a + b")
    , ("1", "\n  return 1\n  2")
    , ("throw:cancel", "\n  throw cancel\n  2")
    ]
  putStrLn "ok"

code_parser_methods = unlines [
    "satisfy f ="
  , "  c = input.0 | eof"
  , "  f(c) || miss"
  , "  input := input.slice(1)"
  , "  c"
  ]

test src tests =  mapM_ go tests
  where
    go (expect, exp) = eq expect (run $ src ++ "\nmain = " ++ exp)
    env = get_env src
    eq expect fact = if expect == fact
      then putStr "."
      else error $ "\n" ++ expect ++ " != " ++ fact
    run src = case lookup "main" (get_env src) of
      Just v -> fmt $ eval (get_env src) v
      _ -> error $ "not found main in " ++ (show $ get_env src)
    get_env src = case parse src of
      (env, "") -> env
      (env, rest) -> error $ "failed parsing: `" ++ rest ++ "`" ++
        "\n  env: " ++ show env ++
        "\n  source: " ++ src

fmt (Void) = "()"
fmt (Bool True) = "true"
fmt (Bool False) = "false"
fmt (Int n) = show n
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