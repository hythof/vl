module Go where

import Common
import System.Process (runCommand)

main = do
  src <- readFile "go.vl"
  sample <- readFile "sample.vl"
  case parse src of
    Left (msg, env) -> do
      putStrLn $ "parse error: " ++ msg
      putStrLn $ fmt_env env
    Right env -> case evaluate env $ (Apply (Ref "compile") [String sample]) of
      Success (String go_src) _ -> do
        let path = "/tmp/tmp.go"
        writeFile path $ go_layout ++ go_src
        runCommand $ "go run " ++ path
        return ()
      Fail msg scope -> do
        putStrLn msg
        putStrLn $ fmt_scope scope
        fail "Compile Error"

 where
  go_layout = unlines [
      "package main"
    , "import \"fmt\""
    , "func main() {"
    , "  ret := vl_main()"
    , "  fmt.Printf(\"%v\\n\", ret)"
    , "}"
    ]
