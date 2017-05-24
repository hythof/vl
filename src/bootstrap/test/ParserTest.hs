module Main where
import           AST
import           Debug.Trace (trace)
import           Parser

check expect fact = if expect == fact then Right "." else Left $ "expect " ++ (show expect) ++ " fact " ++ show fact

ok expect source = case parse top source of
    Left x -> Left $ "Fail parse " ++ source
    Right x -> check expect x

def key expect = case parse file example of
    Left x -> Left $ show x
    Right xs -> case lookup key xs of
        Nothing -> Left $ "Fail lookup " ++ key ++ " " ++ (show xs)
        Just x  -> check expect x

display :: [Either String String] -> IO ()
display xs = do
    putStrLn $ map line xs
    mapM_ detail xs
  where
    line (Left _) = 'F'
    line (Right _) = '.'
    detail (Left x) = putStrLn x
    detail (Right _) = return ()

main = do
    display [
        ok (Int 1) "1"
      , ok (Int 10) "10"
      , ok (Real 1.0) "1.0"
      , ok (Real 10.01) "10.01"
      , ok (Bool True) "T"
      , ok (Bool False) "F"
      , ok (List []) "[]"
      , ok (List [Int 1]) "[1]"
      , ok (List [Apply ["a"] []]) "[a]"
      , ok (List [Apply ["a"] [Int 1]]) "[a 1]"
      , ok (List [Apply ["a"] [Int 1], Apply ["b"] [Int 2]]) "[a 1, b 2]"
      , ok (List [Int 1, Int 2]) "[1, 2]"
      , ok (List [Int 1, Int 2]) "[1\n2]"
      , ok (List [Int 1, Int 2]) "[1\n  \n2]"
      , ok (List [Int 1, Int 2]) "[\n1\n2\n]"
      , ok (String "") "\"\""
      , ok (Struct []) "{}"
      , ok (Struct [("a", Int 1)]) "{a 1}"
      , ok (Struct [("a", Int 1), ("b", Int 2)]) "{a 1, b 2}"
      , ok (Struct [("algebric", Tag "algebric" [] [])]) "{algebric}"
      , ok (Struct [("algebric", Tag "algebric" ["int"] [])]) "{algebric int}"
      , ok (Struct [("algebric", Tag "algebric" ["int", "float"] [])]) "{algebric int float}"
      , ok (Struct [("a", Tag "a" [] []), ("b", Tag "b" [] [])]) "{a | b}"
      , ok (Struct [("a", Tag "a" ["int"] []), ("b", Tag "b" [] [])]) "{a int | b}"
      , ok (Struct [("a", Tag "a" [] []), ("b", Tag "b" ["float"] [])]) "{a | b float}"
      , ok (Struct [("a", Tag "a" ["int"] []), ("b", Tag "b" ["float"] [])]) "{a int | b float}"
      , ok (Apply ["a"] []) "a"
      , ok (Apply ["a", "b"] []) "a.b"
      , ok (Apply ["a", "b", "c"] []) "a.b.c"
      , ok (Apply ["a"] [Int 1]) "(a 1)"
      , ok (Apply ["a"] [Apply ["b"] []]) "(a b)"
      , ok (If (Bool True) (Int 1) (Int 2)) "if T 1 2"
      , ok (Func ["a"] $ Apply ["a"] []) "a = a"
      , ok (Op "+" (Apply ["a"] []) (Apply ["b"] [])) "a+b"
      , ok (Op "+" (Int 1) (Int 2)) "1+2"
      , ok (Op "-" (Int 1) (Int 2)) "1-2"
      , ok (Op "*" (Int 1) (Int 2)) "1*2"
      , ok (Op "/" (Int 1) (Int 2)) "1/2"
      , ok (Op "**" (Int 1) (Int 2)) "1**2"
      , ok (Op "+" (Int 1) (Op "-" (Int 2) (Int 3))) "1 + 2 - 3"
      , ok (Op "-" (Op "+" (Int 1) (Int 2)) (Int 3)) "(1 + 2) - 3"
      , ok (Op "==" (Apply ["a"] []) (Apply ["b"] [])) "a == b"
      , def "a" $ Int 1
      , def "add" $ Func ["a", "b"] (Op "+" (Apply ["a"] []) (Apply ["b"] []))
      , def "transform" $ Op "+" (Apply ["point"] []) (Apply ["order"] [])
      , def "set" $ Apply ["transform"] [Int 1, Int 2, Int 3]
      , def "true" $ Tag "true" [] []
      , def "false" $ Tag "false" [] []
      , def "top" $ Struct [("i", Int 1), ("nest", Struct [("j", Int 2), ("ij", Op "+" (Apply ["i"] []) (Apply ["j"] []))])]
      ]

example = "a 1\n" ++
    "b 2\n" ++
    "id x = x\n" ++
    "add a b = a + b\n" ++
    "point = {x 0, y 0}\n" ++
    "order = {z 0}\n" ++
    "move = (point 1 2)\n" ++
    "transform = point + order\n" ++
    "set = (transform 1 2 3)\n" ++
    "true | false\n" ++
    "top = {i 1, nest {j 2, ij = i + j}}"
