module Go where

import Parser (parse)
import AST
import Evaluator (eval)
import System.Process (runCommand)

main = do
  src <- readFile "go.vl"
  case parse src of
    (env, "") -> case eval env $ (Apply (Ref "compile") [String src]) of
      (String go_src) -> do
        let path = "/tmp/tmp.go"
        writeFile path $ go_src ++ "\n"
        runCommand $ "go run " ++ path
        return ()
      ast -> do
        print ast
        fail "Compile Error"
    (env, msg) -> do
      putStrLn $ "parse error: " ++ msg
      print env
