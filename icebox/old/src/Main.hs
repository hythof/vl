module Main where
import System.IO
import System.Environment
import Parser
import BuildC

main = do
    args <- getArgs
    code <- readFile $ args !! 0
    print code
    print $ parse_program code
