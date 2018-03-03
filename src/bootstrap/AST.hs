module AST where

data AST =
  -- value
    Int    Integer
  | Real   Double
  | Bool   Bool
  | Char   Char
  | String String
  | List   [AST]
  | Struct [AST]
  | Func   [AST] AST -- arguments, return
  | Seq    [AST]
  | Assign String AST
  -- reference
  | Def    [String] [AST] AST -- names, arguments, return
  | Ref    [String] -- names
  | Call   [AST]
  -- others
  | Error  String
  | Comment String
  deriving (Show, Eq)
