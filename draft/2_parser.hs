module AST where

data AST = Noop -- Abstract Syntax Tree
-- value
  | Int    Integer
  | Double Double
  | String String
  | Bool   Char
  | List   [AST]
  | Struct [AST]
-- decl
  | DeclVar  String AST
  | DeclFunc String [String] AST -- name, arguments, return
  | DeclType String AST -- name Struct user_class
-- refrence
  | Lookup [String] [AST] -- name, arguments
-- expr
  | Op String AST AST
  deriving (Show, Eq)
