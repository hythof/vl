module Main where 
import Eval
import Parse
import Define

import Debug.Trace
import Test.HUnit

main = do
    runTestTT testRef

testRef = test [
      ok (Byte 1) "byte"
    , ok (Int 1) "int"
    , ok (Float 1.0) "float"
    , ok (Bool True) "true"
    , ok (Bool False) "false"
    , ok (Char 'a') "char"
    , ok (String "b") "string"
    , ok (Array [Int 1]) "array"
    , ok (Struct [("n", Int 1)]) "struct"
    , ok (Int 1) "struct.n"
    , ok (Func ["a", "b"] (Op2 "+" a b)) "func"
    , ok (Func ["a", "b", "c"] (If a b c)) "check"
    , ok (Func ["a", "b"] (Op2 "+" a b)) "closure2"
    , ok (Int 2) "closure0"
    ]
  where
    src = unlines [
        "byte 0x01"
      , "int 1"
      , "float 1.0"
      , "true T"
      , "false F"
      , "char 'a'"
      , "string \"b\""
      , "array [1]"
      , "struct {n 1}"
      , "func a b = a + b"
      , "check a b c = if a b c"
      , "closure2 a b = a + b"
      , "closure1 a = closure2 1 a"
      , "closure0 = closure1 1"
      ]
    defines = case parse src of
        Right xs -> xs
        Left err -> error $ show err
    ok expect name = (eval defines (Ref name)) ~?= expect
    a = Ref "a"
    b = Ref "b"
    c = Ref "c"
