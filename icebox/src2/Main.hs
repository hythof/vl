module Main where

-- cabal install readline
import System.IO
import System.Console.Readline
import AST
import Parser
import Eval

main :: IO ()
main = do
    prompt []
    return ()

prompt :: [(String, AST)] -> IO ()
prompt scope = do
    hFlush stdout
    code <- readline "> "
    case code of
      Nothing -> return ()
      Just "exit" -> return ()
      Just "quit" -> return ()
      Just line -> prompt_run line
  where
    print_scope (name, ast) = do
      putStr $ "" ++ name ++ " = "
      print ast
    prompt_run "" = do
        prompt scope
    prompt_run "?" = do
        mapM_ print_scope scope
        prompt scope
    prompt_run code = do
        addHistory code
        new_scope <- run scope $ parse_line code
        prompt new_scope

run :: [(String, AST)] -> LineResult -> IO [(String, AST)]
run scope (LineError err) = do
    print err
    return scope
run scope (LineDefine (name, ast)) = return $ (name, ast) : scope
run scope (LineExp value) = do
    printAST scope value
    return scope

printAST :: [(String, AST)] -> AST -> IO ()
printAST scope value = do
    print $ eval scope value
    print value
