module AST where
import Data.Word

-- Abstract Syntax Tree
data AST =
-- value
    Int     Integer
  | Float   Double
  | Bool    Bool
  | String  String
--  | Byte    Word8
--  | Bytes   [Word8]
  | List    [AST]
  | Struct  [(String, AST)]
--  | Tuple   [AST]
  | Func    [String] AST -- arguments, return
-- refrence
  | Lookup  [String] [AST] -- name, arguments
-- exp
  | Op      String AST AST
-- selection
  | If      AST AST AST
-- compile error
  | Error   String
  deriving (Show, Eq)
