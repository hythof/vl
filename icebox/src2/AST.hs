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
  | Algebra [[String]]
-- refrence
  | Lookup [String] [AST] -- name, arguments
-- exp
  | Op String AST AST
-- selection
  | Case [(AST, AST)]
-- compile error
  | Error String
  deriving (Show, Eq)
