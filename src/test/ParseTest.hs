module Main where 
import Parse
import Define

import Text.Parsec hiding (spaces)
import Text.Parsec.String
import Test.HUnit

main = do
    runTestTT testPrimitive
    runTestTT testExpression
    runTestTT testMultiLine

check p expect code = chk expect ast
  where
    ast = parse p "" code
    chk a (Right b) = b ~?= a
    chk a (Left b) = error $ code ++ " :: " ++ (show b)

testPrimitive = test [
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
   ]
 where
   ok = check primitive

testExpression = test [
      ok (PTyped $ Byte 0) "0x00"
    , ok (PTyped $ Int 1) "1"
    , ok (PTyped $ Float 1.0) "1.0"
    , ok (PTyped $ Char 'o')  "'o'"
    , ok (PTyped $ String "hi")  "\"hi\""
    , ok (PTyped $ Bool True) "T"
    , ok (PTyped $ Bool False) "F"
    , ok (PFunc ["id"] (PTyped $ Int 1)) "(id = 1)"
    , ok (PApply "foo" [] top) "foo"
    , ok (PApply "foo" [PApply "bar" [] $ pos 1 5] top) "foo bar"
    , ok (PApply "+" [i1, i2] top) "1+2"
    , ok (PApply "+" [i1, i2] top) "1 + 2"
    , ok (PApply "+" [i1, i2] $ pos 1 2) "(1+2)"
    , ok (PApply "+" [i1, i2] $ pos 1 4) "(((1+2)))"
    , ok (PIf i1 i2 i3) "if 1 2 3"
    , ok (PIf (PApply "==" [i1, i2] $ pos 1 5) i1 i2) "if (1 == 2) 1 2"
    , ok (PArray []) "[]"
    , ok (PArray [(PTyped $ Int 1), (PTyped $ Int 2)]) "[1 2]"
    , ok (PStruct []) "{}"
    , ok (PStruct [
            ("foo", PTyped $ Int 10)
          , ("bar", PFunc [] $ PTyped $ Int 20)
         ])
         "{foo 10 bar=20}"
    , ok (PStruct []) "{}"
    ]
  where
   pos = Source ""
   top = pos 1 1
   i1 = PTyped $ Int 1
   i2 = PTyped $ Int 2
   i3 = PTyped $ Int 3
   ok = check expression

testMultiLine = test [
     ok [
         ("a", i1)
       , ("b", i2)
     ] "a 1\nb 2"
   , ok [
         ("foo", PFunc [] $ apply "bar" [] 1 7)
       , ("bar", PFunc [] $ apply "foo" [] 2 7)
     ] "foo = bar\nbar = foo"
   , ok [
         ("foo", PFunc ["hoge"] $ apply "bar" [apply "hoge" [] 1 16] 1 12)
       , ("bar", PFunc ["hoge"] $ apply "foo" [apply "hoge" [] 2 16] 2 12)
     ] "foo hoge = bar hoge\nbar hoge = foo hoge"
    ]
  where
   i1 = PTyped $ Int 1
   i2 = PTyped $ Int 2
   apply name inner line colmun = PApply name inner (pos line colmun)
   pos = Source "multi"
   ok expect code = case program code "multi" of
       (Right x) -> x ~?= expect
       (Left x) -> error $ code ++ " :: " ++ (show x)
