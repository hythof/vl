module AST where

data AST =
  -- value
    Int    Integer
  | Real   Double
  | Bool   Bool
  | Char   Char
  | String String
  | List   [AST]
  | Struct [(String, AST)]
  | Func   [String] AST -- arguments, return
  | Seq    [AST]
  | Assign String AST
  -- reference
  | Ref    String
  | Call   [AST]
  -- others
  | Error  String
  deriving (Show, Eq)
