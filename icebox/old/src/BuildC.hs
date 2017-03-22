module BuildC where

import System.IO
import System.Cmd
import Data.Monoid
import Data.List
import Control.Monad.Writer
import Parser
import AST

-- records
data CProg = CProg {
    struct :: String,
    decl :: String,
    func :: String
} deriving (Eq, Ord, Show, Read)

instance Monoid CProg where
    mempty = CProg "" "" ""
    mappend (CProg s1 d1 f1) (CProg s2 d2 f2) = CProg (s1 ++ s2) (d1 ++ d2) (f1 ++ f2)

-- utility
tell_struct v = tell CProg { struct=v, decl="", func="" }
tell_decl v = tell CProg { struct="", decl=v, func="" }
tell_func v = tell CProg { struct="", decl="", func=v ++ " " }
tell_decl_func v = tell CProg { struct="", decl=v, func=v }
tt :: AST -> String
tt (Double _) = "double"
tt (Int _) = "int"
tt (String _) = "char*"
tt (Bool _) = "char"
tt (List _) = "void*"
tt (Struct _) = "void*"
tt (Noop) = "void"

-- build C code
build :: AST -> Writer CProg ()
build Noop = tell mempty
build (Double n) = tell_func $ show n
build (Int n) = tell_func $ show n
build (String s) = tell_func $ "\"" ++ s ++ "\""
build (Bool 'T') = tell_func "1"
build (Bool 'F') = tell_func "0"
build (Op op left right) = do
    build left
    tell_func op
    build right

build (LookupVar name) = tell_func $ foldl1 _add name
  where
    _add a b  = a ++ "." ++ b

build (DeclVar name value) = do
    tell_func $ tt value ++ name ++ " = "
    build value

build (DeclFunc fn args ret (Struct stms)) = do
    tell_decl_func $ tt ret
    tell_decl_func " "
    tell_decl_func fn
    tell_decl_func "("
    tell_decl_func $ to_func_args args
    tell_decl_func ")"
    tell_decl ";"
    tell_func "{"
    mapM_ build stms
    tell_func ";"
    tell_func "}"

build (DeclFunc a b c d) = build $ DeclFunc (to_func_name [a]) b c (Struct [d])
build (Struct ast_list) = mapM_ build ast_list
build (LookupFunc names args) = do
    tell_func $ to_func_name names
    tell_func "("
    mapM_ build args
    tell_func ")"

to_func_name names = "app_" ++ foldl1 _join names
  where
    _join a b = a ++ "__" ++ b

to_func_args [] = "vm_t *vm"
to_func_args args = "vm_t *vm, void *" ++ intercalate ", void *" args

---- -- process.vl
---- get "/" = html {
----     html = "index page"
---- }
---- 
---- get "/list/" = html {
----     page 1
----     show 10
----     html = "<h1>books</h1>" ++ book_list
----     bool_list = DB.get_book_list.map => book {
----         book.title ++ ": " ++ book.published_at
----     }.join("<br>")
----     unreachable_function = "do not call, delete when compile"
---- }
---- 
---- -- dispatch.vl
---- get = @process.get
---- get "/"
---- get "/list/"
---- get "/list/"
---- get "/list/" page:1 show:30
---- get "/list/" cgi.params
---- get "/list/" page:cgi.params("params").int
---- get "/list/" book_list:"comming soon"
---- 
---- -- main.vl
---- cgi = @cgi
---- dispatch.get

--build (DeclVar name (Struct ast_list)) = do
--    TI.writeFile ("/tmp/x.c") $ TL.pack "hello"
--build (DeclVar name (Int n)) = do
--    head <- get
--    put $ head ++ concat ["int ", name, ";\n"]
--    return $ concat ["name = ", show n, ";"]
--
--builds :: [AST] -> Monad.State String String
--builds [] = return ""
--builds (c:cs) = do
--    return $ (build c) ++ (build cs)
-- conv (Int n) = show n
-- conv (Double n) = show n
-- conv (String s) = concat ["\"", s, "\""]
-- conv (Bool 'T') = "1"
-- conv (Bool 'F') = "0"
-- conv prefix (DeclVar name (Int n)) = concat ["int ", prefix, name, " = ", show n, ";"]
-- conv prefix (DeclVar name (Double n)) = concat ["double ", prefix, name, " = ", show n, ";"]
-- conv prefix (DeclVar name (String s)) = concat ["char *", prefix, name, " = \"",  s, "\";"]
-- conv prefix (DeclVar name (Bool 'T')) = concat ["char ", prefix, name, " = 1;"]
-- conv prefix (DeclVar name (Bool 'F')) = concat ["char ", prefix, name, " = 0;"]
