module Main where

import AST
import Debug.Trace (trace)
import Control.Applicative ((<|>))
import Control.Monad (guard)

-- Parse --------------------------------------------------
parse :: String -> Maybe (AST, Source)
parse s = runParser parse_exp $ Source s 0 (length s)

parse_exp :: Parser AST
parse_exp = go
  where
    go :: Parser AST
    go = do
      left <- parse_int
      (parse_op2 left) <|> (return left)
    parse_op2 :: AST -> Parser AST
    parse_op2 left = do
      op <- read_op
      right <- parse_exp
      return $ Call op [left, right]
parse_int :: Parser AST
parse_int = do
  s <- many1 (satisfy ((flip elem) "0123456789"))
  return $ Int (read s :: Int)

read_op :: Parser String
read_op = do
  c <- satisfy ((flip elem) "+-*/%")
  return [c]

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> do
  guard $ pos s < len s
  let c = (src s) !! (pos s)
  guard $ f c
  let br = c == '\n'
  let pos' = pos s + 1
  return (c, s { pos = 1 + pos s })
many :: Parser a -> Parser [a]
many f = many_acc f []
many1 :: Parser a -> Parser [a]
many1 f = do
  x <- f
  xs <- many f
  return $ x : xs
many_acc :: Parser a -> [a] -> Parser [a]
many_acc f acc = (do
  x <- f
  many_acc f (x : acc)
  ) <|> (return $ reverse acc)

-- Eval ---------------------------------------------------
eval :: AST -> AST
eval x@(Int _) = x
eval (Call op [left, right]) = go op (eval left) (eval right)
  where
    go "+" (Int l) (Int r) = Int $ l + r
    go "-" (Int l) (Int r) = Int $ l - r
    go "*" (Int l) (Int r) = Int $ l * r
    go "/" (Int l) (Int r) = Int $ l `div` r
    go "%" (Int l) (Int r) = Int $ l `mod` r
eval x = error $ "Failed evaluation: " ++ show x

-- Main ---------------------------------------------------
test :: AST -> String -> IO ()
test expect input = do
  case parse input of
    Nothing -> error $ "Parser error " ++ input ++ " expect: " ++ show expect
    Just (x, s) -> if pos s == len s
      then if expect == (eval x) then putChar '.' else putStrLn $ "x\n- expect: " ++ show expect ++ "\n-   fact: " ++ show (eval x) ++ " :: " ++ show x
      else error $ "Parser error remaining " ++ (drop (pos s) (src s))

main = do
  test (Int 1) "1"
  test (Int 12) "12"
  test (Int 5) "2+3"
  test (Int $ -1) "2-3"
  test (Int 6) "2*3"
  test (Int 0) "2/3"
  test (Int 2) "2%3"
  putStrLn "done"
