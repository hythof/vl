module Define where

data PT =
    PTyped  AST
  | PFunc   [String] PT
  | PApply  String [PT] Source
  | PIf     PT PT PT
  | PArray  [PT]
  | PStruct [(String, PT)]
  deriving (Show, Eq)

data AST =
    Byte     Int
  | Int      Integer
  | Float    Double
  | Bool     Bool
  | Char     Char
  | String   String
  | Array    [AST]
  | Tuple    [AST]
  | Struct   [(String, AST)]
  | Instance String [AST]
  | Func     AST [String]
  | Apply    String [AST] Source
  | If       AST AST AST
  | Error    String Source
  deriving (Show, Eq)

data Source = Source String Int Int
  deriving (Show, Eq)

data Type =
    TUndef
  | TByte
  | TInt
  | TFloat
  | TBool
  | TChar
  | TString
  | TArray  Type
  | TTuple  [Type]
  | TClass  [(String, Type)]
  | TFunc   [Type]
  | TApply  [Type]
  deriving (Show, Eq)
