module AST where

data AST =
    Int    Integer
  | Real   Double
  | Bool   Bool
  | Char   Char
  | String String
  | List   [AST]
  | Struct [AST]
  | Func   [AST] AST -- arguments, return
  | Type   String [AST] AST -- name, arguments, return
  | Def    [String] [AST] AST -- names, arguments, return
  | Ref    [String] -- names
  | Call   [AST]
  | Comment String
  | Namespace String
  | Import [[String]]
  | Export [[String]]
  | Error  String
  deriving (Show, Eq)
