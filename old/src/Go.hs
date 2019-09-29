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
        run_test env
        let path = "/tmp/tmp.go"
        writeFile path $ go_layout ++ go_src ++ "\n"
        runCommand $ "go run " ++ path
        return ()
      Fail msg scope -> do
        putStrLn msg
        putStrLn $ fmt_scope scope
        fail "Compile Error"

go_layout = unlines [
    "package main"
  , "import \"fmt\""
  , "import \"strconv\""
  , "type vt struct {}"
  , "func main() {"
  , "  var v vt"
  , "  ret := v.main()"
  , "  fmt.Printf(\"%v\\n\", ret)"
  , "}"
  ]

run_test :: Env -> IO ()
run_test env = do
  putStrLn $ to_s $ evaluate env (Apply (Ref "parse") [String "hell"])
 where
  to_s v = case v of
    Success (String s) _ -> s
    Success v _ -> show v
    Fail msg _ -> msg
