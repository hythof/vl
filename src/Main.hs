module Main where

import AST
import Data.Traversable (forM)
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
  return $ I64 (read s :: Int)

read_op :: Parser String
read_op = op2 <|> op1
  where
    op1 = satisfy ((flip elem) "+-*/%") >>= \c -> return [c]
    op2 = string ":="
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
char :: Char -> Parser Char
char c = satisfy (== c)
string :: String -> Parser String
string target = Parser $ \s -> do
  guard $ target == take (length target) (drop (pos s) (src s))
  return (target, s { pos = length target + pos s })
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

compile :: [AST] -> String
compile xs = go
  where
    go = ll_main ++ ll_suffix
    lines = do
      rs <- mapM (line . optimize) xs
      let r = last rs
      emit $ "ret i" ++ ty r ++ " " ++ reg r
    ll_main = compileToLL "v_main" "64" lines
    line :: AST -> Compiler Register
    line (I64 n) = assign "64" (show n)
    line (Def name ast) = define name $ optimize ast
    line (Call name []) = reference name
    line (Call ":=" [op1@(Call name []), op2]) = do
      o1 <- line op1
      o2 <- line op2
      let ty1 = ty o1
      let ty2 = ty o2
      let ty = if ty1 == ty2 then ty1 else error $ "Type miss match op: := left:" ++ show op1 ++ " right " ++ show op2
      n <- store ty (mem o1) (reg o2)
      n <- load ty n
      register name (Register ty n n)
    line (Call op [left, right]) = do
      l <- line left
      r <- line right
      let tyl = ty l
      let tyr = ty r
      let ty = if tyl == tyr then tyl else error $ "Type miss match op: " ++ op ++ " left:" ++ show l ++ " right " ++ show r
      let rl = reg l
      let rr = reg r
      n <- case op of
        "+" -> next $ "add i" ++ ty ++ " " ++ rl ++ ", " ++ rr
        "-" -> next $ "sub i" ++ ty ++ " " ++ rl ++ ", " ++ rr
        "*" -> next $ "mul i" ++ ty ++ " " ++ rl ++ ", " ++ rr
        "/" -> next $ "sdiv i" ++ ty ++ " " ++ rl ++ ", " ++ rr
        "%" -> next $ "srem i" ++ ty ++ " " ++ rl ++ ", " ++ rr
      return $ Register ty n n

    ll_line x = error $ show x
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
    store ty n v = (emit $ "store i" ++ ty ++ " " ++ v ++ ", i" ++ ty ++ "* " ++ n ++ ", align 4") >> return n
    load ty n = next $ "load i" ++ ty ++ ", i" ++ ty ++ "* " ++ n ++ ", align 4"
    assign ty v = do
      r1 <- next $ "alloca i" ++ ty ++ ", align 4"
      store ty r1 v
      r2 <- load ty r1
      return $ Register ty r2 r1
    define name v = case v of
      I64 x -> assign "64" (show x) >>= \n -> register name n
      _ -> error $ "Does not define " ++ show v
    compileToLL name ty f = let
      (_, d) = runCompile f (Define 0 [] [])
      in "define i" ++ ty ++ "  @" ++ name ++ "() #0 {\n" ++ (unlines (reverse $ body d)) ++ "\n}\n"

optimize :: AST -> AST
optimize ast = go ast
  where
    go (Call op [I64 l, I64 r]) = op2 op l r
    go x@(Call op [left, right]) = case (go left, go right) of
      (I64 l, I64 r) -> op2 op l r
      (l, r) -> Call op [l, r]
    go x = x
    op2 op l r = case op of
      "+" -> I64 $ l + r
      "-" -> I64 $ l - r
      "*" -> I64 $ l * r
      "/" -> I64 $ l `div` r
      "%" -> I64 $ l `mod` r
      _ -> error $ "Unsupported operator: " ++ op ++ " left: " ++ show l ++ " right: " ++ show r

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
  test "5" "x=2\ny=3\nx+y"
  test "-1" "x=2\ny=3\nx-y"
  test "6" "x=2\ny=3\nx*y"
  test "0" "x=2\ny=3\nx/y"
  test "2" "x=2\ny=3\nx%y"
  test "3" "x=2\nx:=3\nx"
  test "3" "x=2\nx:=3\ny=4\nx"
  putStrLn "done"
