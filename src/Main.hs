module Main where

import AST
import Data.Traversable (forM)
import Debug.Trace (trace)
import Control.Applicative ((<|>))
import Control.Monad (guard)
import System.Process (system)

all_exec_ops = [":=", "+", "-", "*", "/", "%", ">=", ">", "<=", "<", "!=", "==", "&&", "||"]
all_parse_ops = ["+=", "-=", "*=", "/=", "%="] ++ all_exec_ops

-- Parse --------------------------------------------------
parse :: String -> Maybe ([AST], Source)
parse s = runParser parse_top $ Source s 0 (length s)
parse_top = sep_by1 parse_line read_br1
parse_line = parse_type <|> parse_def <|> parse_exp
parse_type = do
  name <- read_id
  args <- many (read_spaces1 >> read_id)
  read_spaces
  defs <- between (char '{' >> read_separator) (read_separator >> char '}') (sep_by parse_def read_br1)
  return $ Struct name (map (\(def@(Def name args lines)) -> (name, def)) defs)
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
parse_unit :: Parser AST
parse_unit = go
  where
    go = do
      x <- parenthis <|> parse_string <|> parse_int <|> parse_bool <|> parse_call
      sub_part x
    sub_part x = nest_part x <|> (return x)
    nest_part x = do
      char '.'
      name <- read_id <|> error "Invalid method calling format"
      argv <- between (char '(' >> read_spaces) (read_spaces >> char ')') (sep_by parse_unit read_spaces1) <|> (return [])
      sub_part $ Call name (x : argv)
    parenthis = between (char '(') (char ')') parse_exp
    parse_string = do
      s <- between (char '"') (char '"') (many $ satisfy (\c -> c /= '"'))
      return $ String s
    parse_int = do
      s <- many1 (satisfy ((flip elem) "0123456789"))
      return $ I64 (read s :: Int)
    parse_bool = do
      s <- string "true" <|> string "false"
      return $ Bool (s == "true")
    parse_call = do
      name <- read_id
      argv <- between (char '(' >> read_spaces) (read_spaces >> char ')') (sep_by parse_unit read_spaces1) <|> (return [])
      return $ Call name argv

read_op :: Parser String
read_op = op2 <|> op1
  where
    op1 = satisfy ((flip elem) "+-*/%") >>= \c -> return [c]
    op2 = try_op2 all_parse_ops
    try_op2 [] = Parser $ \_ -> Nothing
    try_op2 (x:xs) = (string x) <|> try_op2 xs
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
data CompiledResult = CompiledResult {
    cr_reg :: Register,
    cr_code :: String,
    cr_def :: Define
  }

eval :: [AST] -> IO String
eval x = do
  system $ "mkdir -p /tmp/llvm"
  let ll_main = compile x
  ll_lib <- readFile "./lib.ll"
  let ll = ll_lib ++ ll_main
  writeFile "/tmp/llvm/v.ll" ll
  system $ "(cd /tmp/llvm && lli v.ll > stdout.txt)"
  readFile "/tmp/llvm/stdout.txt"

compile :: [AST] -> String
compile top_lines = go
  where
    go = let
      cr = c_main
      in ll_prefix cr ++ "\n; user functions\n" ++ cr_code cr ++ "\n" ++ ll_suffix cr
    call_ref name argv = ref top_lines
      where
        ref [] = error $ "Not found " ++ name ++ " with " ++ show argv
        ref (x@(Def name' args body):xs) = if name == name' then x else ref xs
        ref (_:xs) = ref xs
    c_lines lines = do
      rs <- mapM (c_line . optimize) lines
      return $ last rs
    c_func lines = do
      r <- c_lines lines
      emit $ "ret " ++ rty r ++ " " ++ reg r
      return r
    c_main = compile_func "v_main" [] [] (c_func top_lines)
    c_line :: AST -> Compiler Register
    c_line x@(I64 n) = assign x (show n)
    c_line x@(Bool True) = assign x "true"
    c_line x@(Bool False) = assign x "false"
    c_line x@(String s) = do
      let l = show $ length s
      sn <- inc_string s
      n <- next $ "load i8*, i8** " ++ sn ++ ", align 8"
      return $ Register x "i8*" n sn
    c_line (Def name [] [line]) = define name line
    c_line (Def name [] lines) = do
      r <- c_lines lines
      n <- assign (ast r) (reg r)
      register name n
    c_line (Def name args lines) = noop
    c_line (Call name []) = reference name
    c_line (Call "if" [cond, op1, op2]) = do
      -- if condition
      rc <- c_line cond
      l1 <- inc_label >>= \n -> return $ "label" ++ show n
      l2 <- inc_label >>= \n -> return $ "label" ++ show n
      l3 <- inc_label >>= \n -> return $ "label" ++ show n
      emit $ "br i1 " ++ (reg rc) ++ ", label %" ++ l1 ++ ", label %" ++ l2
      -- if block 1
      emit $ "\n" ++ l1 ++ ":"
      o1 <- c_line op1
      emit $ "br label %" ++ l3
      -- if block 2
      emit $ "\n" ++ l2 ++ ":"
      o2 <- c_line op2
      emit $ "br label %" ++ l3
      -- finish branch
      emit $ "\n" ++ l3 ++ ":"
      -- finilize
      if rty o1 == rty o2 then return () else error $ "Not match types on if 1: " ++ show o1 ++ " 2:" ++ show o2
      n <- next $ "phi " ++ rty o1 ++ " [ " ++ reg o1 ++ ", %" ++ l1 ++ " ], [ " ++ reg o2 ++ ", %" ++ l2 ++ " ]"
      return $ Register (ast o1) (rty o1) n ""
    c_line (Call op [op1, op2]) = if elem op all_exec_ops
      then c_op2 op op1 op2
      else c_call op [op1, op2]
    c_line (Call name args) = c_call name args
    c_line (Struct name defs) = c_def name defs
    c_line x = error $ "Unsupported compiling: " ++ show x
    c_def name defs = trace ("No implement yet define: " ++ name) noop
    c_call :: String -> [AST] -> Compiler Register
    c_call name argv = go
      where
        go :: Compiler Register
        go = do
          registers <- mapM c_line argv
          switch name (map ast registers) registers
        switch :: String -> [AST] -> [Register] -> Compiler Register
        switch "nth" [s@(String _), I64 _] [r1, r2] = do
          new <- next $ "tail call i8* @si64_nth(i8* " ++ reg r1 ++ ", " ++ "i64 " ++ reg r2 ++ ")"
          return (Register (String "") (aty $ String "") new (new ++ "*"))
        switch "length" [s@(String _)] [r1] = do
          new <- next $ "tail call i64 @strlen(i8* " ++ reg r1 ++ ")"
          return (Register (I64 0) (aty $ I64 0) new (new ++ "*"))
        switch "append" [s@(String _), v@(String _)] [r1, r2] = do
          new <- next $ "tail call i8* @ss_append(i8* " ++ reg r1 ++ ", i8* " ++ reg r2 ++ ")"
          return (Register (String "") (aty $ String "") new (new ++ "*"))
        switch "prefix" [s@(String _), (I64 _)] [r1, r2] = do
          new <- next $ "tail call i8* @si64_prefix(i8* " ++ reg r1 ++ ", i64 " ++ reg r2 ++ ")"
          return (Register (String "") (aty $ String "") new (new ++ "*"))
        switch "slice" [s@(String _), (I64 _), (I64 _)] [r1, r2, r3] = do
          new <- next $ "tail call i8* @si64i64_slice(i8* " ++ reg r1 ++ ", i64 " ++ reg r2  ++ ", i64 " ++ reg r3 ++ ")"
          return (Register (String "") (aty $ String "") new (new ++ "*"))
        switch _ _ registers = do
          let (Def _ args lines) = call_ref name argv
          global_strings <- get_strings
          let env = zip args registers
          let cr = compile_func name env global_strings $ c_func lines
          let r = cr_reg cr
          let code = cr_code cr
          define_sub code
          let call_argv = string_join ", " $ map (\r -> rty r ++ " " ++ reg r) registers
          n <- next $ "call " ++ rty r ++ " @" ++ name ++ "(" ++ call_argv ++ ")"
          return $ rcopy r n
    c_op2 op op1 op2 = do
      o1 <- c_line op1
      o2 <- c_line op2
      let r1 = reg o1
      let r2 = reg o2
      let ty1 = rty o1
      let ty2 = rty o2
      let ty = if ty1 == ty2 then ty1 else error $ "Type miss match op: := left:" ++ show op1 ++ " right " ++ show op2
      let op_code code = (next $ code ++ " " ++ ty ++ " " ++ r1 ++ ", " ++ r2) >>= \n -> return $ rcopy o1 n
      case op of
        ":=" -> do
            n <- store ty (mem o1) (reg o2)
            n <- load ty n
            let (Call name []) = op1
            register name (rcopy o1 n)
        "+" -> op_code "add"
        "-" -> op_code "sub"
        "*" -> op_code "mul"
        "/" -> op_code "sdiv"
        "%" -> op_code "srem"
        _ -> error $ "Unsupported op: " ++ op
    ll_prefix cr = go
      where
        go = unlines [
            "; prefix"
          , string_join "\n"  $ strings (cr_def cr)
          , "@.d_format = private unnamed_addr constant [3 x i8] c\"%d\00\", align 1"
          , "@.true_format = private unnamed_addr constant [5 x i8] c\"true\00\", align 1"
          , "@.false_format = private unnamed_addr constant [6 x i8] c\"false\00\", align 1"
          , "@.s_format = private unnamed_addr constant [3 x i8] c\"%s\00\", align 1"
          , "@.bug_format = private unnamed_addr constant [4 x i8] c\"BUG\00\", align 1"
          ]
    ll_suffix cr = go
      where
        r = cr_reg cr
        go = unlines [
            "; suffix"
          , "define i32 @main() #0 {"
          , "  %1 = alloca i32, align 4"
          , "  store i32 0, i32* %1, align 4"
          , "  %2 = call " ++ rty r ++ " @v_main()"
          , printf $ ast r
          , "  ret i32 0 "
          , "}"
          ]
        printf (I64 _) = "  call void @i64_printf(i64 %2)"
        printf (Bool _) = "  %3 = sext i1 %2 to i8\ncall void @bool_printf(i8 %3)"
        printf (String _) = "  call void @s_printf(i8* %2)"
        printf x = error $ "No implement print function ast: " ++ show x
    noop = return $ Register Void "" "" ""
    store ty n v = (emit $ "store " ++ ty ++ " " ++ v ++ ", " ++ ty ++ "* " ++ n ++ ", align 4") >> return n
    load ty n = next $ "load " ++ ty ++ ", " ++ ty ++ "* " ++ n ++ ", align 4"
    assign a v = do
      let ty = aty a
      r1 <- next $ "alloca " ++ ty ++ ", align 4"
      store ty r1 v
      r2 <- load ty r1
      return $ Register a ty r2 r1
    aty a = case a of
      (I64 _) -> "i64"
      (Bool _) -> "i1"
      (String _) -> "i8*"
      _ -> error $ "Untyped " ++ show a
    define name v = case v of
      I64 x -> assign v (show x) >>= \n -> register name n
      Bool x -> assign v (if x then "true" else "false") >>= \n -> register name n
      String s -> do
        let l = show $ length s
        sn <- inc_string s
        n <- next $ "load i8*, i8** " ++ sn ++ ", align 8"
        register name $ Register v "i8*" n sn
      Call _ _ -> do
        r <- c_line v
        register name r
      _ -> error $ "Does not define " ++ show v
    compile_func :: String -> [(String, Register)] -> [String] -> Compiler Register -> CompiledResult
    compile_func name env global_strings f = let
      env' = map (\(i, (name, r)) -> (name, rcopy r ("%" ++ show i))) (zip [0..] env)
      (r, d) = runCompile f (Define (length env') 0 global_strings env' [] [])
      sub_funcs = unlines $ subs d
      argv = string_join "," $ map (\(_, r) -> rty r) env
      define_code = "define " ++ (rty r) ++ " @" ++ name ++ "(" ++ argv ++ ") #0 {\n" ++ (unlines (reverse $ body d)) ++ "}\n" ++ sub_funcs
      in CompiledResult r define_code d

optimize :: AST -> AST
optimize ast = unwrap_synatx_sugar $ constant_folding ast
constant_folding ast = go ast
  where
    go (Call op [I64 l, I64 r]) = op2_int op l r
    go (Call op [Bool l, Bool r]) = op2_bool op l r
    go x@(Call op [left, right]) = case (go left, go right) of
      (I64 l, I64 r) -> op2_int op l r
      (l, r) -> Call op [l, r]
    go x = x
    op2_int op l r = case op of
      "+" -> I64 $ l + r
      "-" -> I64 $ l - r
      "*" -> I64 $ l * r
      "/" -> I64 $ l `div` r
      "%" -> I64 $ l `mod` r
      ">" -> Bool $ l > r
      ">=" -> Bool $ l >= r
      "<" -> Bool $ l < r
      "<=" -> Bool $ l <= r
      "==" -> Bool $ l == r
      "!=" -> Bool $ not (l == r)
      _ -> Call op [I64 l, I64 r]
    op2_bool op l r = case op of
      "&&" -> Bool $ l && r
      "||" -> Bool $ l || r
      _ -> Call op [Bool l, Bool r]
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
      else putStrLn $ "x\n- expect: " ++ show expect ++ "\n-   fact: " ++ show stdout ++ " :: " ++ show ast

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
  test "true" "true"
  test "false" "false"
  test "true" "true && true"
  test "false" "false && true"
  test "true" "true || true"
  test "true" "false || true"
  test "true" "1 == 1"
  test "false" "1 == 2"
  test "false" "1 != 1"
  test "true" "1 != 2"
  test "true" "1 >= 1"
  test "false" "1 > 1"
  test "true" "1 <= 1"
  test "false" "1 < 1"
  test "1" "if(true 1 2)"
  test "2" "if(false 1 2)"
  test "1" "x=0\nif(true 1 (2/x))"
  test "1" "x=0\nif(false (2/x) 1)"
  test "a" "\"a\""
  test "hi" "\"hi\""
  test "a1" "x=\"a1\"\nx"
  test "h" "\"hi\".nth(0)"
  test "h" "x=\"hi\"\nx.nth(0)"
  test "i" "x=\"hi\"\nx.nth(1)"
  test "1" "\"h\".length"
  test "1" "x=\"h\"\nx.length"
  test "2" "x=\"hi\"\nx.length"
  test "1" "x=1\ny=x\ny"
  test "h" "x=\"h\"\ny = x.append(\"i\")\nx"
  test "hi" "x=\"h\"\ny = x.append(\"i\")\ny"
  test "hi" "x=\"h\"\ny = \"i\"\nx.append(y)"
  test "h" "\"hi\".nth(0)"
  test "h" "\"hi\".prefix(1)"
  test "hi" "\"hi\".prefix(2)"
  test "hi" "\"hi\".prefix(3)"
  test "h" "\"hi\".slice(0 1)"
  test "i" "\"hi\".slice(1 1)"
  test "hi" "\"hi\".slice(0 2)"
  test "hi" "\"hi\".slice(0 3)"
  test "void()" "void {}\nvoid"
  putStrLn "done"
