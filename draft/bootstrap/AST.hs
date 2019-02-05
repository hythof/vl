module AST where

data AST =
  -- value
    Char   Char
  | Int    Integer
  | Real   Double
  | List   [AST]
  | Bool   Bool
  | String String
  | Func   [String] AST -- arguments, return
  | Struct [(String, AST)]
  | Recursive [[String]]
  | Instance String [(String, AST)]
  | Call   [AST]
  | Seq    [AST]
  | Assign String AST
  | Ref    [String]
  | Error  String
  deriving (Show, Eq)
