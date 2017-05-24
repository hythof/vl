module Define where

data AST =
    Byte     Int
  | Int      Integer
  | Float    Double
  | Bool     Bool
  | Char     Char
  | String   String
  | Array    [AST]
  | Struct   [(String, AST)]
  | Class    String [[String]]
  | New      String [String]
  | Match    String [String]
  | Instance String [AST]
  | Func     [String] AST
  | Closure  [(String, AST)] AST
  | Op2      String AST AST
  | Ref      String
  | Apply    String [AST]
  | If       AST AST AST
  | Case     AST [(AST, AST)] AST
  | Error    String
  deriving (Show, Eq)
