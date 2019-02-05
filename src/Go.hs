module Go where

import Common
import System.Process (runCommand)

main = do
  src <- readFile "go.vl"
  sample <- readFile "sample.vl"
  let env = parse src
  let ret = eval env $ (Apply (Ref "compile") [String sample])
  case ret of
    String go_src -> do
      let path = "/tmp/tmp.go"
      writeFile path go_src
      runCommand $ "go run " ++ path
    ret -> do
      dump_env env
      print $ show ret
      fail "Compile Error"
