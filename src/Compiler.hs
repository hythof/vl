module Compiler (compile) where

import Base
import Debug.Trace (trace)

data CompiledResult = CompiledResult {
    cr_reg :: Register,
    cr_code :: String,
    cr_def :: Define
  }

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
    c_def name defs = do
      let argv = [] -- TODO refer defs
      sn <- inc_string (name ++ "(" ++ argv ++ ")")
      let sname = "%_struct." ++ name
      let fname = "@new_" ++ name
      let body = unlines [
                   "  %1 = tail call i8* @malloc(i64 0)"
                 , "  %2 = bitcast i8* %1 to " ++ sname ++ "*"
                 , "  ret " ++ sname ++ "* %2"
                 ]
      let n = show $ 3 + length name
      let print_body = unlines [
                         "  %2 = load i8*, i8** " ++ sn ++ ", align 8"
                       , "  %3 = tail call i32 (i8*, ...) @printf(i8* %2)"
                       , "  ret void"
                       ]
      let new_code = "define " ++ sname ++ "* " ++ fname ++ "(" ++ argv ++ ") local_unnamed_addr #0 {\n" ++ body ++ "}\n"
      let print_code = "define void @struct_" ++ name ++ "_printf(" ++ sname ++ "*) local_unnamed_addr #0 {\n" ++ print_body ++ "}\n"
      let struct_code = sname ++ " = type { }"
      define_sub struct_code
      define_sub new_code
      define_sub print_code
      new <- next $ "tail call " ++ sname ++ "* " ++ fname ++ "(" ++ argv ++ ")"
      register name $ Register (Struct name defs) (sname ++ "*") new (new ++ "*")
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
        printf (Struct name _) = "  call void @struct_" ++ name ++ "_printf(%_struct." ++ name ++ "* %2)"
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

--( Optimize before compiling )-------------------------------------------------
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
