module Main where

import System.IO
import System.Console.Readline
import Control.Applicative ((<$>))
import Text.Parsec
import AST
import Eval
import Parser

data LineResult =
    LineError String
  | LineDefine (String, AST)
  | LineExp AST
  deriving Show

main :: IO ()
main = do
    console []
    return ()

console :: [(String, AST)] -> IO ()
console scope = do
    hFlush stdout
    code <- readline "> "
    case code of
      Nothing -> return ()
      Just "exit" -> return ()
      Just "quit" -> return ()
      Just line -> consoleLine line
  where
    printScope (name, ast) = do
      putStr $ "" ++ name ++ " = "
      print ast

    consoleLine "" = do
        console scope
    consoleLine "?" = do
        mapM_ printScope scope
        console scope
    consoleLine code = do
        addHistory code
        newScope <- run scope $ parseLine code
        console newScope

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

parseLine :: String -> LineResult
parseLine code = 
  case parse (parseAny) "" code of
    Left err -> LineError $ show err
    Right res -> res
  where
    parseAny = do
        spaces
        ok <- try func <|> exp
        spaces
        rest <- many $ noneOf []
        case rest of
          "" -> return $ ok
          _ -> return $ LineError $ "ParseError : " ++ rest ++ " ok=" ++ (show ok)
    func = LineDefine <$> parseDeclFunc
    exp = do
        e <- parseExp
        return $ LineExp e
