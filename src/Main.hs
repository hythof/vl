module Main where

import AST
import Debug.Trace (trace)
import Control.Applicative ((<|>))
import Control.Monad (guard)
import System.Process (system)

-- Parse --------------------------------------------------
parse :: String -> Maybe ([AST], Source)
parse s = runParser parse_top $ Source s 0 (length s)

parse_top = sep_by1 (parse_def <|> parse_exp) read_br

parse_def = do
  name <- read_id
  char '='
  body <- parse_exp
  return $ Def name body
parse_exp :: Parser AST
parse_exp = go
  where
    go :: Parser AST
    go = do
      left <- parse_unit
      (exp_op_remaining left) <|> (return left)
    exp_op_remaining :: AST -> Parser AST
    exp_op_remaining left = do
      op <- read_op
      right <- parse_exp
      return $ Call op [left, right]
parse_unit = go
  where
    go = parenthis <|> parse_int <|> parse_call
    parenthis = between (char '(') (char ')') parse_exp
parse_call = do
  name <- read_id
  return $ Call name []
parse_int :: Parser AST
parse_int = do
  s <- many1 (satisfy ((flip elem) "0123456789"))
  return $ Int (read s :: Int)

read_op :: Parser String
read_op = do
  c <- satisfy ((flip elem) "+-*/%")
  return [c]
read_id :: Parser String
read_id = do
  xs <- many1 $ satisfy ((flip elem) "abcdefghijklmnopqrstuvwxyz_")
  ys <- many $ satisfy ((flip elem) "abcdefghijklmnopqrstuvwxyz_0123456789")
  return $ xs ++ ys
read_spaces :: Parser String
read_spaces = many $ satisfy ((flip elem) "\t ")
read_br :: Parser ()
read_br = do
  read_spaces
  char '\n'
  many $ satisfy ((flip elem) "\n\t ")
  return ()

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
sep_by1 :: Parser a -> Parser b -> Parser [a]
sep_by1 body sep = do
  x <- body
  xs <- many (sep >> body)
  return $ x : xs
char c = satisfy (== c)
between l r c = do
  l
  v <- c <|> error "failed in between on the center"
  r <|> error "faield in between on the right"
  return $ v

-- Eval ---------------------------------------------------
eval :: [AST] -> IO String
eval x = do
  system $ "mkdir -p /tmp/llvm"
  let ll = compile x
  writeFile "/tmp/llvm/v.ll" ll
  system $ "(cd /tmp/llvm && lli v.ll > stdout.txt)"
  readFile "/tmp/llvm/stdout.txt"

compile xs = go
  where
    go = ll_main ++ ll_suffix
    body = mapM_ (ll_body . optimize) xs
    ll_main = compileToLL "v_main" 64 (body >> return ())
    ll_body (Int n) = assign 64 (show n)
    ll_body (Def name ast) = define name $ optimize ast
    ll_body (Call name []) = reference name
    ll_body x = error $ show x
    ll_suffix = unlines [
        ""
      , "; common suffix"
      , "@.str = private unnamed_addr constant [3 x i8] c\"%d\00\", align 1"
      , ""
      , "define i32 @main() #0 {"
      , "  %1 = alloca i32, align 4"
      , "  store i32 0, i32* %1, align 4"
      , "  %2 = call i64 @v_main()"
      , "  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str, i32 0, i32 0), i64 %2)"
      , "  ret i32 0 "
      , "}"
      , ""
      , "declare i32 @printf(i8*, ...) #1"
      ]

optimize :: AST -> AST
optimize ast = go ast
  where
    go (Call op [Int l, Int r]) = case op of
      "+" -> Int $ l + r
      "-" -> Int $ l - r
      "*" -> Int $ l * r
      "/" -> Int $ l `div` r
      "%" -> Int $ l `mod` r
    go x@(Call op [left, right]) = case (go left, go right) of
      (Int l, Int r) -> go $ Call op [Int l, Int r]
      (l, r) -> Call op [l, r]
    go x = x

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
  test "1" "x=1\nx"
  --test "5" "x=2\ny=3\nx+y"
  putStrLn "done"
