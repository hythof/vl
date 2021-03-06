module Main where 
import Parse
import Define

import Test.HUnit

main = do
    runTestTT testExpression
    runTestTT testMultiline
    runTestTT testSpec

testExpression = test [
      ok (Byte 0) "0x00"
    , ok (Byte 15) "0x0f"
    , ok (Byte 240) "0xf0"
    , ok (Byte 255) "0xff"
    , ok (Int 1) "1"
    , ok (Float 1.0) "1.0"
    , ok (Char 'o')  "'o'"
    , ok (String "hi")  "\"hi\""
    , ok (Bool True) "T"
    , ok (Bool False) "F"
    , ok (Func ["id"] (Int 1)) "(id = 1)"
    , ok (Ref "foo") "foo"
    , ok (Apply "foo" [Ref "bar"]) "foo bar"
    , ok (Op2 "+" i1 i2) "1+2"
    , ok (Op2 "+" i1 i2) "1 + 2"
    , ok (Op2 "+" i1 i2) "(1+2)"
    , ok (Op2 "+" i1 i2) "(((1+2)))"
    , ok (If i1 i2 i3) "if 1 2 3"
    , ok (If foo foo foo) "if foo foo foo"
    , ok (If (Op2 "==" i1 i2) i1 i2) "if (1 == 2) 1 2"
    , ok (Case (Int 1) [(i1, i1), (i2, i2)] i3) "case 1\n 1 : 1\n  2 : 2\n  3"
    , ok (Array []) "[]"
    , ok (Array [(Int 1), (Int 2)]) "[1 2]"
    , ok (Struct []) "{}"
    , ok (Struct [
            ("foo", Int 10)
          , ("bar", Func [] $ Int 20)
         ])
         "{foo 10 bar=20}"
    , ok (Struct []) "{}"
    ]
  where
    i1 = Int 1
    i2 = Int 2
    i3 = Int 3
    foo = Ref "foo"
    ok expect src = (eval src) ~?= expect
    eval src = case parse ("foo = " ++ src) of
      Right (("foo", Func [] ast):[]) -> ast
      Left x -> Error $ show x ++ " # " ++ src

testMultiline = test [
     ok [
         ("a", i1)
       , ("b", i2)
     ] "a 1\nb 2"
   , ok [
         ("foo", Func [] $ Ref "bar")
       , ("bar", Func [] $ Ref "foo")
     ] "foo = bar\nbar = foo"
   , ok [
         ("foo", Func ["hoge"] $ Apply "bar" [Ref "hoge"])
       , ("bar", Func ["hoge"] $ Apply "foo" [Ref "hoge"])
     ] "foo hoge = bar hoge\nbar hoge = foo hoge"
   ]
  where
   i1 = Int 1
   i2 = Int 2
   ok expect code = case parse code of
       (Right x) -> x ~?= expect
       (Left x) -> error $ code ++ " :: " ++ (show x)

testSpec = test [
     ok [
         ("spec_byte", Byte 1)
       , ("spec_int", Int 1)
       , ("spec_float", Float 1.0)
       , ("spec_true", Bool True)
       , ("spec_false", Bool False)
       , ("spec_char", Char 'a')
       , ("spec_string", String "b")
       , ("spec_array", Array [Int 1])
       , ("spec_struct", Struct [("n", Int 1)])
       , ("spec_func", Func ["a", "b"] (Op2 "+" (Ref "a") (Ref "b")))
       , ("check", Func ["a", "b", "c"] (If (Ref "a") (Ref "b") (Ref "c")))
       , ("AB", Class "AB" [["A"], ["B", "int"]])
       , ("spec_a", (Func [] $ Ref "AB.A"))
       , ("spec_b", (Func [] $ Apply "AB.B" [Int 1]))
       , ("spec_c", (Func [] $ Case
           (Ref "spec_b")
           [(Match "AB.A" [], Int 0), (Match "AB.B" ["n"], Ref "n")]
           (Int 2)))
     ] src
   ]
  where
    ok expect code = case parse code of
       (Right x) -> x ~?= expect
       (Left x) -> error $ code ++ " :: " ++ (show x)
    src = unlines [
        "spec_byte 0x01"
      , "spec_int 1"
      , "spec_float 1.0"
      , "spec_true T"
      , "spec_false F"
      , "spec_char 'a'"
      , "spec_string \"b\""
      , "spec_array [1]"
      , "spec_struct {n 1}"
      , "spec_func a b = a + b"
      , "check a b c = if a b c"
      , "AB |\n A\n B int"
      , "spec_a = AB.A"
      , "spec_b = AB.B 1"
      , "spec_c = case spec_b\n AB.A : 0\n AB.B n : n\n 2"
      ]
