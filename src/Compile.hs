module Compile where

import           Boot              (parse, run, show_env)
import           Control.Exception (finally)

main :: IO ()
main = do
  src <- readFile "c.vl"
  putStrLn "-- result"
  (putStrLn $ run src) `finally` do
    putStrLn "-- env"
    putStrLn $ show_env $ parse src
