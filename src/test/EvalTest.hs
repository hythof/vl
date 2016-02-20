module Main where 
import Text.Parsec (parse)
import Test.HUnit
import Parser
import Eval
import AST

main = do
    runTestTT testOp
    runTestTT testCase
    runTestTT testLookup
--    runTestTT testPatternMatch

check scope p expect code = chk expect ast
    where
        ast = parse p "" code
        chk a (Right b) = (eval scope b) ~?= a
        chk a (Left b) = error $ code ++ " :: " ++ (show b)

parseDefines src = case parseProgram src of
    Left _ -> []
    Right x -> x

testOp = test [
          ok (Int 3) "1 + 2"
        , ok (Int (-1)) "1 - 2"
        , ok (Int 6) "2 * 3"
        , ok (Int 2) "4 // 2"
        , ok (Int 1) "5 % 2"
        , ok (Int 9) "3 ** 2"
        , ok (Int 4) "1 << 2"
        , ok (Int 2) "8 >> 2"
        , ok (Int 3) "1 | 2"
        , ok (Int 3) "3 & 7"
        , ok (Float 3.0) "1.0 + 2.0"
        , ok (Float (-1.0)) "1.0 - 2.0"
        , ok (Float 6.0) "2.0 * 3.0"
        , ok (Float 2.0) "4.0 / 2.0"
        , ok (Float 9.0) "3.0 ** 2.0"
        , ok (String "hello world") "\"hello \" + \"world\""
        , ok (List [Int 1, Int 2]) "[1] + [2]"
        , ok (Bool True) "T & T"
        , ok (Bool False) "T & F"
        , ok (Bool False) "F & T"
        , ok (Bool False) "F & F"
        , ok (Bool True) "T | T"
        , ok (Bool True) "T | F"
        , ok (Bool True) "F | T"
        , ok (Bool False) "F | F"
        ]
    where
        ok = check [] parseExp

testCase = test [
          ok (Int 1) "if T then 1 else 2"
        , ok (Int 2) "if F then 1 else 2"
        , ok (Int 2) "if 1 == 1 then 2 else 3"
        , ok (Int 4) "if 1 == 2 then 3 else 4"
        ]
    where
        ok = check [] parseExp

testLookup = test [
          ok (Int 3) "a + b"
        , ok (Int 31) "person.age"
        , ok (String "foo") "person.name"
        , ok (String "bar") "person.nest.name"
        , ok (String "foobar") "person.name + person.nest.name"
        , ok (Int 34) "person.nest.id"
        , ok (Int 5) "add 2 3"
        , ok (Int 3) "add a b"
        , ok (Int 1) "id a"
        , ok (Int 2) "id(b)"
        , ok (Int 3) "add (id a) (id b)"
        , ok (Int 4) "add (id a) (add (id a) (id b))"
        , ok (Struct [("x", Int 1), ("y", Int 2)]) "move"
        , ok (Int 1) "move.x"
        , ok (Int 2) "move.y"
        , ok (Struct [("x", Int 0), ("y", Int 0), ("z", Int 0)]) "transform"
        , ok (Int 0) "transform.x"
        , ok (Int 0) "transform.y"
        , ok (Int 0) "transform.z"
        , ok (Int 1) "set.x"
        , ok (Int 2) "set.y"
        , ok (Int 3) "set.z"
        ]
    where
        ok = check src parseExp
        src = parseDefines ("a 1\n" ++
            "b 2\n" ++
            "id x = x\n" ++
            "add a b = a + b\n" ++
            "point = {x 0 y 0}\n" ++
            "order = {z 0}\n" ++
            "move = point 1 2\n" ++
            "transform = point + order\n" ++
            "set = transform 1 2 3\n" ++
            "person = {age 31 name \"foo\" nest { name \"bar\" id = age + b + a } }")

testPatternMatch = test [
          ok (Int 1) "match 1"
        , ok (Float 2) "match 1.0"
        , ok (Int 3) "match \"string\""
        ]
    where
        ok = check src parseExp
        src = parseDefines ("match x = case x of\n" ++
            "  Int = 1\n" ++
            "  Float = 2\n" ++
            "  String = 3")
