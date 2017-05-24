module AST where
-- Abstract Syntax Tree

data AST =
-- value
    Int    Integer
  | Real   Double
  | Bool   Bool
  | List   [AST]
  | String String
  | Struct [(String, AST)]
  | Func   [String] AST -- arguments, return
  | Tag    String [String] [AST]
-- refrence
  | Apply  [String] [AST] -- names, arguments
  | Op String AST AST
-- selection
  | If AST AST AST
-- runtime
  | Error String
  deriving (Show, Eq)
