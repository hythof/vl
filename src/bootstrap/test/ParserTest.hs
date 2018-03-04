module Main where
import           AST
import           Debug.Trace (trace)
import           Parser

check expect fact source = if expect == fact
  then Right "."
  else Left $ "expect: " ++ (show expect) ++ "\n  fact: " ++ show fact ++ "\nsource: " ++ show source

ok expect source = case parse ast_exp source of
    Left x -> Left $ "Fail parse " ++ source
    Right x -> check expect [x] source

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
       ok [Int 1] "1"
     , ok [Int 10] "10"
     , ok [Real 1.0] "1.0"
     , ok [Real 10.01] "10.01"
     , ok [Bool True] "true"
     , ok [Bool False] "false"
     , ok [List []] "[]"
     , ok [List [Int 1]] "[1]"
     , ok [List [Ref "a" ]] "[a]"
     , ok [List [Call [Ref "a", Int 1]]] "[a 1]"
     , ok [List [Int 1, Int 2]] "[1; 2]"
     , ok [List [Int 1, Int 2]] "[1\n2]"
     , ok [List [Int 1, Int 2]] "[1\n  \n2]"
     , ok [List [Int 1, Int 2]] "[\n1\n2\n]"
     , ok [String ""] "\"\""
     , ok [Struct []] "{}"
     , ok [Struct [("a", Ref "int")]] "{a : int}"
     , ok [Struct [("id", Func ["a"] (Ref "a"))]] "{id a : a}"
     , ok [Struct [("a", Ref "int"),
                   ("id", Func ["a"] (Ref "a"))]] "{a : int; id a : a}"
     , ok [Ref "a"] "a"
     , ok [Call [Call [Ref "a", Ref "b"], Ref "c"]] "a.b.c"
     , ok [Call [Ref "a", Int 1]] "(a 1)"
     , ok [Call [Ref "a", Ref "b"]] "(a b)"
     , ok [Call [Ref "if", Bool True, Int 1, Int 2]] "if true 1 2"
     , ok [Call [Ref "+", Int 1, Int 2]] "1 + 2"
     , ok [Call [Ref "//", Int 1, Int 2]] "1 // 2"
     , ok [Func ["a"] $ Ref "a"] "a => a"
     , ok [Seq [Assign "a" $ Ref "f"]] "(do a<=f)"
     , ok [Seq [Assign "a" $ Ref "f", Ref "a"]] "(do a <= f\na)"
      ]
