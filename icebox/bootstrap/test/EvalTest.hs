module Main where 
import Eval
import Parse
import Define

import Debug.Trace
import Test.HUnit

main = do
    runTestTT testRef
    runTestTT testDefine

testRef = test [
      ok (Byte 1) "0x01"
    , ok (Int 1) "1"
    , ok (Float 1.0) "1.0"
    , ok (Bool True) "T"
    , ok (Bool False) "F"
    , ok (Int 1) "if T 1 2"
    , ok (Int 2) "if F 1 2"
    , ok (Int 10) "case 1\n 1 : 10\n 2: 20\n 30"
    , ok (Int 20) "case 2\n 1 : 10\n 2: 20\n 30"
    , ok (Int 30) "case 9\n 1 : 10\n 2: 20\n 30"
    , ok (Char 'a') "'a'"
    , ok (String "b") "\"b\""
    , ok (Array [Int 1]) "[1]"
    , ok (Struct [("n", Int 1)]) "{n 1}"
    ]
  where
    defines src = case parse ("foo = " ++ src) of
        Right xs -> xs
        Left err -> error $ show err
    ok expect src = (eval (defines src) (Ref "foo")) ~?= expect

testDefine = test [
      ok (Int 1) "struct.n"
    , ok (Func ["a", "b"] (Op2 "+" a b)) "func"
    , ok (Func ["a", "b", "c"] (If a b c)) "check"
    , ok (Func ["a", "b"] (Op2 "+" a b)) "closure2"
    , ok (Int 2) "closure0"
    , ok (Class "AB" [["A"], ["B", "int"]]) "AB"
    , ok (Instance "AB.A" []) "ab_a"
    , ok (Instance "AB.B" [Int 1]) "ab_b"
    , ok (Int 0) "ab_c1"
    , ok (Int 1) "ab_c2"
    , ok (Int 2) "ab_c3"
    ]
  where
    src = unlines [
        "struct {n 1}"
      , "func a b = a + b"
      , "check a b c = if a b c"
      , "closure2 a b = a + b"
      , "closure1 a = closure2 1 a"
      , "closure0 = closure1 1"
      , "AB |\n A\n B int"
      , "ab_a = AB.A"
      , "ab_b = AB.B 1"
      , "ab_c1 = case ab_a\n AB.A : 0\n AB.B n : n\n 2"
      , "ab_c2 = case ab_b\n AB.A : 0\n AB.B n : n\n 2"
      , "ab_c3 = case 3\n AB.A : 0\n AB.B n : n\n 2"
      ]
    defines = case parse src of
        Right xs -> xs
        Left err -> error $ show err
    ok expect name = (eval defines (Ref name)) ~?= expect
    a = Ref "a"
    b = Ref "b"
    c = Ref "c"
