module Compile where

import Boot hiding(main)


main :: IO ()
main = do
  source <- readFile "c.vl"
  putStrLn $ run_for_dev source
