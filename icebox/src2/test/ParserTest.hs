module Main where 
--import Text.Parsec (parse)
import Text.Parsec hiding (spaces)
import Text.Parsec.String

import Test.HUnit
import Parser
import AST

main = do
    runTestTT testValue
    runTestTT testLookup
    runTestTT testIf
    runTestTT testSwitch
    runTestTT testExp
    runTestTT testConst
    runTestTT testFunc
    runTestTT testAlgebra
    runTestTT testMultiLine

check p expect code = chk expect ast
    where
        ast = parse p "" code
        chk a (Right b) = b ~?= a
        chk a (Left b) = error $ code ++ " :: " ++ (show b)

testValue = test [
          ok (Int 100) "100"
        , ok (Real 100.0) "100.0"
        , ok (String "hi")  "\"hi\""
        , ok (Bool True) "T"
        , ok (Bool False) "F"
        , ok (List []) "[]"
        , ok (List [Int 1, Int 2, Int 3]) "[1 2 3]"
        , ok (List [String "hello", String "world"]) "[\"hello\" \"world\"]"
        , ok (Struct []) "{}"
        , ok (Struct [("foo", Int 10)]) "{foo 10}"
        , ok (Struct [("foo", Int 10)]) "{foo=10}"
        , ok (Struct [
                ("foo", Int 10)
              , ("bar", Int 20)
             ])
             "{foo 10 bar=20}"
        ]
    where
        ok = check parse_value

testIf = test [
          ok (Case [((Op "==" (Int 1) (Int 2)), (Int 3)), (Bool True, (Int 4))]) "if 1 == 2 then 3 else 4"
        ]
    where
        ok = check parse_if

testSwitch = test [
          ok (Case [
                ((Op "==" (Int 1) (Int 1)), (Int 10)), 
                ((Op "==" (Int 1) (Int 2)), (Int 20)), 
                (Bool True, (Int 30))]
            )
            "switch 1\n1 : 10\n2 : 20\n_ : 30"
        ]
    where
        ok = check parse_switch

testLookup = test [
          ok (Lookup ["var"] []) "var"
        , ok (Lookup ["foo", "bar"] []) "foo.bar"
        , ok (Lookup ["foo", "bar"] [Int 1]) "foo.bar 1"
        , ok (Lookup ["foo", "bar"] [Int 1, Int 2]) "foo.bar 1 2"
        , ok (Lookup ["foo", "bar"] [Int 1, Int 2, Int 3]) "foo.bar 1 2 3"
        , ok (Lookup ["foo", "bar"] [Int 1, String "foo"]) "foo.bar 1 \"foo\""
        , ok (Lookup ["add"] [(Lookup ["x"] []), (Lookup ["y"] [])]) "add x y"
        ]
    where
        ok = check parse_factor

testExp = test [
          ok (Op "+" (Int 1) (Int 2)) "1+2"
        , ok (Op "-" (Int 1) (Int 2)) "1 - 2"
        , ok (Op "*" (Int 1) (Int 2)) "1 * 2"
        , ok (Op "/" (Int 1) (Int 2)) "1 / 2"
        , ok (Op "%" (Int 1) (Int 2)) "1 % 2"
        , ok (Op "**" (Int 1) (Int 2)) "1 ** 2"
        , ok (Op "//" (Int 1) (Int 2)) "1 // 2"
        , ok (Op "|" (Int 1) (Int 2)) "1 | 2"
        , ok (Op "&" (Int 1) (Int 2)) "1 & 2"
        , ok (Op "<<" (Int 1) (Int 2)) "1 << 2"
        , ok (Op ">>" (Int 1) (Int 2)) "1 >> 2"
        , ok (Op "/" (Real 1) (Real 2)) "1.0 / 2.0"
        , ok (Op "-" (Op "+" (Int 1) (Op "*" (Int 2) (Int 3))) (Int 4)) "1 + 2 * 3 - 4"
        , ok (Op "+" (String "a") (String "b")) "\"a\" + \"b\""
        , ok (Op "+" (Lookup ["a"] []) (Lookup ["b"] [])) "a + b"
        ]
    where
        ok = check parse_exp

testConst = test [
          ok ("foo", Int 1) "foo 1"
        , ok ("too", Struct [
              ("foo", Int 10)
            , ("bar", Int 20)
           ])
           "too {foo 10 bar=20}"
        , ok ("too", Struct [
              ("foo", Int 10)
            , ("bar", Int 20)
           ])
           "too {  foo 10 bar = 20  }"
    ]
    where
        ok = check parse_const

testFunc = test [
          ok ("a", Int 1) "a=1"
        , ok ("f", Int 1) "f=1"
        , ok ("f", Int 1) "f= 1"
        , ok ("f", Int 1) "f =1"
        , ok ("f", Int 1) "f = 1"
        , ok ("f", Struct []) "f = {}"
        , ok ("f", Func ["a"] (Int 1)) "f a = 1"
        , ok ("f", Func ["a", "b"] (Int 1)) "f a b = 1"
        , ok ("f", Func ["a", "b"] (Op "+" (Lookup ["a"] []) (Lookup ["b"] []))) "f a b = a + b"
        , ok ("f", Func ["a", "b"] (Op "+" (Lookup ["a"] []) (Lookup ["b"] []))) "f a b = a + b"
        --, ok (DeclFunc "main" [] (Struct []) (List [])) "main = [ env.console.puts 1 ]"
    ]
    where
        ok = check parse_func

testAlgebra = test [
        ok (Algebra [["Foo"], ["Bar"]]) "Foo | Bar"
      , ok (Algebra [["Left", "Int"], ["Right", "String"]]) "Left Int | Right String"
    ]
    where
        ok = check parse_algebra

testMultiLine = test [
          ok [
              ("a", Int 1)
            , ("b", Int 2)
          ] "a 1\nb 2"
        , ok [
              ("a", Lookup ["b"] [])
            , ("b", Int 1)
          ] "a = b\nb = 1"
        , ok [
              ("a", Lookup ["b"] [Lookup ["a"] []])
            , ("b", Func ["x"] (Lookup ["x"] []))
          ] "a = b a\nb x = x"
        , ok [
              ("a", Lookup ["b"] [String "a"])
            , ("b", Func ["x"] (Lookup ["x"] []))
          ] "a = b \"a\"\nb x = x"
        , ok [
              ("a", Lookup ["b"] [Int 1])
            , ("b", Func ["x"] (Lookup ["x"] []))
          ] "a = b 1\nb x = x"
        , ok [
              ("a", Lookup ["b"] [Int 1, Int 2])
            , ("b", Func ["x", "y"] (Op "+" (Lookup ["x"] []) (Lookup ["y"] [])))
          ] "a = b 1 2\nb x y = x + y"
    ]
    where
        ok expect code = (parse_program code) ~?= expect
