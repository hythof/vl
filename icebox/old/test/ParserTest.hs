module Main where 
import Text.Parsec (parse)
import Test.HUnit
import Parser
import AST

check p expect code = chk expect ret
    where
        ret = (parse p "" code)
        chk a (Right b) = b ~?= a

testValue = test [
          ok (Int 100) "100"
        , ok (Double 100.0) "100.0"
        , ok (String "hi")  "\"hi\""
        , ok (Bool 'T') "T"
        , ok (Bool 'F') "F"
        , ok (List []) "[]"
        , ok (List [Int 1, Int 2, Int 3]) "[1 2 3]"
        , ok (List [String "hello", String "world"]) "[\"hello\" \"world\"]"
        , ok (Struct []) "{}"
        , ok (Struct [DeclVar "foo" (Int 10)]) "{foo 10}"
        , ok (Struct [DeclFunc "foo" [] (Int 10)]) "{foo=10}"
        , ok (Struct [
                DeclVar "foo" (Int 10)
              , DeclFunc "bar" [] (Int 20)
             ])
             "{foo 10 bar=20}"
        , ok (Lookup ["var"] []) "var"
        , ok (Lookup ["foo", "bar"] []) "foo.bar"
        , ok (Lookup ["foo", "bar"] [Int 1]) "foo.bar 1"
        , ok (Lookup ["foo", "bar"] [Int 1, String "foo"]) "foo.bar 1 \"foo\""
        ]
    where
        ok = check parse_value

testExp = test [
          ok (Op "+" (Int 1) (Int 2)) "1+2"
        , ok (Op "-" (Int 1) (Int 2)) "1 - 2"
        , ok (Op "*" (Int 1) (Int 2)) "1 * 2"
        , ok (Op "/" (Int 1) (Int 2)) "1 / 2"
        , ok (Op "-" (Op "+" (Int 1) (Op "*" (Int 2) (Int 3))) (Int 4)) "1 + 2 * 3 - 4"
        ]
    where
        ok = check parse_exp

testDeclVar = test [
          ok (DeclVar "foo" (Int 1)) "foo 1"
        , ok (DeclVar "too" (Struct [
                DeclVar "foo" (Int 10)
              , DeclFunc "bar" [] (Int 20)
             ]))
             "too {foo 10 bar=20}"
        , ok (DeclVar "too" (Struct [
                DeclVar "foo" (Int 10)
              , DeclFunc "bar" [] (Int 20)
             ]))
             "too {  foo 10 bar = 20  }"
    ]
    where
        ok = check parse_var

testDeclFunc = test [
          ok (DeclFunc "foo" [] (Int 1)) "foo=1"
        , ok (DeclFunc "foo" [] (Int 1)) "foo = 1"
        , ok (DeclFunc "foo" [] (Struct [])) "foo = {}"
        , ok (DeclFunc "foo" ["a", "b"] (Int 1)) "foo a b = 1"
        , ok (DeclFunc "foo" ["a", "b"] (Op "+" (Lookup ["a"] []) (Lookup ["b"] []))) "foo a b = a + b"
        --, ok (DeclFunc "main" [] (Struct []) (List [])) "main = [ env.console.puts 1 ]"
        ]
    where
        ok = check parse_func

testAll = do
    runTestTT testValue
    runTestTT testExp
    runTestTT testDeclVar
    runTestTT testDeclFunc
