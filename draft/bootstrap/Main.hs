module Main where
import Eval
import Parser
import AST

main = do
    src <- getContents
    case run src "main" of
        Left err -> putStrLn err
        Right (scope, result) -> do
            putStrLn "-- main"
            putStrLn $ show result
            putStrLn "-- test"
            putStrLn $ run_test
            putStrLn "-- src"
            putStr src
            putStrLn "-- ast"
            mapM_ (\(k, v) -> putStrLn $ k ++ "\t" ++ show v) scope
          where
            run_test = case lookup "test" scope of
                Nothing -> "(no test)"
                Just (Func [arg] ast) -> show $ eval ((arg, tester) : scope) ast
                  where
                    tester = case parse top "{eq a b = if (a == b) \".\" \"F\"}" of
                        Left x -> Error $ show x
                        Right x -> x

run src root = do
    xs <- parse file src
    case lookup root xs of
        Nothing -> Left "not found main()"
        Just main -> Right $ (xs, eval xs main)
