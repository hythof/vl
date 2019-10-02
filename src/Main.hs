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
parse_top = sep_by1 parse_line read_br1
parse_line = parse_def <|> parse_exp
parse_def = do
  name <- read_id
  args <- many (read_spaces1 >> read_id)
  read_spaces
  char '='
  read_spaces
  lines <- parse_stmt <|> (parse_exp >>= \exp -> return [exp])
  return $ Def name args lines
parse_stmt = between (char '{' >> read_separator) (read_separator >> char '}') (sep_by parse_line read_br1)
parse_exp :: Parser AST
parse_exp = go
  where
    go :: Parser AST
    go = do
      left <- parse_unit
      (exp_op_remaining left) <|> (return left)
    exp_op_remaining :: AST -> Parser AST
    exp_op_remaining left = do
      read_spaces
      op <- read_op
      read_spaces
      right <- parse_exp
      return $ Call op [left, right]
parse_unit = go
  where
    go = parenthis <|> parse_int <|> parse_call
    parenthis = between (char '(') (char ')') parse_exp
parse_call = do
  name <- read_id
  args <- between (char '(' >> read_spaces) (read_spaces >> char ')') (sep_by parse_unit read_spaces1) <|> (return [])
  return $ Call name args
parse_int :: Parser AST
parse_int = do
  s <- many1 (satisfy ((flip elem) "0123456789"))
  return $ I64 (read s :: Int)

read_op :: Parser String
read_op = op2 <|> op1
  where
    op1 = satisfy ((flip elem) "+-*/%") >>= \c -> return [c]
    op2 = string ":=" <|>
          string "+=" <|>
          string "-=" <|>
          string "*=" <|>
          string "/=" <|>
          string "%="
read_id :: Parser String
read_id = do
  xs <- many1 $ satisfy ((flip elem) "abcdefghijklmnopqrstuvwxyz_")
  ys <- many $ satisfy ((flip elem) "abcdefghijklmnopqrstuvwxyz_0123456789")
  return $ xs ++ ys
read_spaces :: Parser String
read_spaces = many $ satisfy ((flip elem) "\t ")
read_spaces1 :: Parser String
read_spaces1 = many1 $ satisfy ((flip elem) "\t ")
read_separator :: Parser String
read_separator = many $ satisfy ((flip elem) "\n\t ")
read_br1 :: Parser ()
read_br1 = do
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
sep_by :: Parser a -> Parser b -> Parser [a]
sep_by body sep = (sep_by1 body sep) <|> return []
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
compile top_lines = go
  where
    go = c_main ++ ll_suffix
    all_op = [":=", "+", "-", "*", "/", "%"]
    call_ref name argv = ref top_lines
      where
        ref [] = error $ "Deos not found " ++ name ++ " with " ++ show argv
        ref (x@(Def name' args _):xs) = case (name == name', length args == length argv) of
          (True, True) -> x
          (True, False) -> error $ "Does not match number of arguments expect=" ++ (show $ length args) ++ " given=" ++ show argv
          _ -> ref xs
        ref (_:xs) = ref xs
    c_lines lines = do
      rs <- mapM (c_line . optimize) lines
      return $ last rs
    c_func lines = do
      r <- c_lines lines
      emit $ "ret i" ++ rty r ++ " " ++ reg r
      return r
    c_main = snd $ compile_func "v_main" [] (c_func top_lines)
    c_line :: AST -> Compiler Register
    c_line (I64 n) = assign "64" (show n)
    c_line (Def name [] [line]) = define name line
    c_line (Def name [] lines) = do
      r <- c_lines lines
      n <- assign (rty r) (reg r)
      register name n
    c_line (Def name args lines) = noop
    c_line (Call name []) = reference name
    c_line (Call op [op1, op2]) = if elem op all_op
      then c_op2 op op1 op2
      else c_call op [op1, op2]
    c_line (Call name args) = c_call name args
    c_line x = error $ "Unsupported compiling: " ++ show x
    c_call name argv = do
      let ast = call_ref name argv
      let (Def _ args lines) = ast
      registers <- mapM c_line argv
      let env = zip args registers
      let (r, code) = compile_func name env $ c_func lines
      define_sub code
      let call_argv = string_join ", " $ map (\r -> "i" ++ rty r ++ " " ++ reg r) registers
      n <- next $ "call i" ++ rty r ++ " @" ++ name ++ "(" ++ call_argv ++ ")"
      return $ Register (rty r) n ""
    c_op2 op op1 op2 = do
      o1 <- c_line op1
      o2 <- c_line op2
      let r1 = reg o1
      let r2 = reg o2
      let ty1 = rty o1
      let ty2 = rty o2
      let ty = if ty1 == ty2 then ty1 else error $ "Type miss match op: := left:" ++ show op1 ++ " right " ++ show op2
      let op_code code = (next $ code ++ " i" ++ ty ++ " " ++ r1 ++ ", " ++ r2) >>= \n -> return $ Register ty n n
      case op of
        ":=" -> do
            n <- store ty (mem o1) (reg o2)
            n <- load ty n
            let (Call name []) = op1
            register name (Register ty n n)
        "+" -> op_code "add"
        "-" -> op_code "sub"
        "*" -> op_code "mul"
        "/" -> op_code "sdiv"
        "%" -> op_code "srem"
        _ -> error $ "Unsupported op: " ++ op
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
    noop = return $ Register "" "" ""
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
    compile_func :: String -> [(String, Register)] -> Compiler Register -> (Register, String)
    compile_func name env f = let
      env' = map (\(i, (name, r)) -> (name, Register (rty r) ("%" ++ show i) "")) (zip [0..] env)
      (r, d) = runCompile f (Define (length env') env' [] [])
      sub_funcs = unlines $ subs d
      argv = string_join "," $ map (\(_, r) -> "i" ++ rty r) env
      in (r, "define i" ++ (rty r) ++ " @" ++ name ++ "(" ++ argv ++ ") #0 {\n" ++ (unlines (reverse $ body d)) ++ "}\n" ++ sub_funcs)

optimize :: AST -> AST
optimize ast = unwrap_synatx_sugar $ constant_folding ast
constant_folding ast = go ast
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
      _ -> Call op [I64 l, I64 r]
unwrap_synatx_sugar ast = go ast
  where
    go (Call "+=" [left, right]) = Call ":=" [left, Call "+" [left, go right]]
    go (Call "-=" [left, right]) = Call ":=" [left, Call "-" [left, go right]]
    go (Call "*=" [left, right]) = Call ":=" [left, Call "*" [left, go right]]
    go (Call "/=" [left, right]) = Call ":=" [left, Call "/" [left, go right]]
    go (Call "%=" [left, right]) = Call ":=" [left, Call "%" [left, go right]]
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
  test "5" "x=2\ny=3\nx+y"
  test "-1" "x=2\ny=3\nx-y"
  test "6" "x=2\ny=3\nx*y"
  test "0" "x=2\ny=3\nx/y"
  test "2" "x=2\ny=3\nx%y"
  test "3" "x=2\nx:=3\nx"
  test "3" "x=2\nx:=3\ny=4\nx"
  test "5" "x=2\nx+=3\nx"
  test "-1" "x=2\nx-=3\nx"
  test "6" "x=2\nx*=3\nx"
  test "0" "x=2\nx/=3\nx"
  test "2" "x=2\nx%=3\nx"
  test "1" "x={1}\nx"
  test "3" "x={y=1\ny+=2\ny}\nx"
  test "1" "id x = x\nid(1)"
  test "5" "add a b = a + b\nadd(2 3)"
  test "9" "add a b c = a + b + c\nadd(2 3 4)"
  putStrLn "done"
