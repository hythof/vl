module Main where

import AST
import Debug.Trace (trace)
import Control.Applicative ((<|>))
import Control.Monad (guard)
import System.Process (system)

-- Parse --------------------------------------------------
parse :: String -> Maybe (AST, Source)
parse s = runParser parse_exp $ Source s 0 (length s)

parse_exp :: Parser AST
parse_exp = go
  where
    go :: Parser AST
    go = do
      left <- parenthis <|> parse_int
      (parse_op2 left) <|> (return left)
    parenthis = between (char '(') (char ')') parse_exp
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
char c = satisfy (== c)
between l r c = do
  l
  v <- c <|> error "failed in between on the center"
  r <|> error "faield in between on the right"
  return $ v

-- Eval ---------------------------------------------------
eval :: AST -> IO String
eval x = do
  system $ "mkdir -p /tmp/llvm"
  let ll = compile x
  writeFile "/tmp/llvm/v.ll" ll
  system $ "(cd /tmp/llvm && lli v.ll > stdout.txt)"
  readFile "/tmp/llvm/stdout.txt"

compile x = go
  where
    go = ll_main ++ ll_suffix
    root = constant_folding x
    ll_main = compileToLL "v_main" 32 (ll_body root >> return ())
    ll_body (Int n) = assign 32 (show n)
    ll_suffix = unlines [
        ""
      , "; common suffix"
      , "@.str = private unnamed_addr constant [3 x i8] c\"%d\00\", align 1"
      , ""
      , "define i32 @main() #0 {"
      , "  %1 = alloca i32, align 4"
      , "  store i32 0, i32* %1, align 4"
      , "  %2 = call i32 @v_main()"
      , "  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str, i32 0, i32 0), i32 %2)"
      , "  ret i32 0 "
      , "}"
      , ""
      , "declare i32 @printf(i8*, ...) #1"
      ]

constant_folding :: AST -> AST
constant_folding ast = go ast
  where
    go x@(Int n) = x
    go (Call "+" [Int l, Int r]) = Int $ l + r
    go (Call "-" [Int l, Int r]) = Int $ l - r
    go (Call "*" [Int l, Int r]) = Int $ l * r
    go (Call "/" [Int l, Int r]) = Int $ l `div` r
    go (Call "%" [Int l, Int r]) = Int $ l `mod` r
    go (Call op [left, right]) = go $ Call op [l, r]
      where
        l = go left
        r = go right

-- Main ---------------------------------------------------
test :: String -> String -> IO ()
test expect input = go
  where
    go = do
      case parse input of
        Nothing -> error $ "Parser error " ++ input ++ " expect: " ++ expect
        Just (x, s) -> if pos s == len s
          then run x
          else error $ "Parser error\n- remaining: " ++ (drop (pos s) (src s)) ++ "\n- src: " ++ input
    run ast = do
      stdout <- eval ast
      if expect == stdout
      then putChar '.'
      else putStrLn $ "x\n- expect: " ++ show expect ++ "\n-   fact: " ++ stdout ++ " :: " ++ show ast

main = do
  test "1" "1"
  test "12" "12"
  test "5" "2+3"
  test "-1" "2-3"
  test "6" "2*3"
  test "0" "2/3"
  test "2" "2%3"
  test "9" "2+3+4"
  test "14" "2+(3*4)"
  test "20" "(2+3)*4"
  putStrLn "done"
