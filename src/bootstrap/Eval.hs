module Eval (eval) where

import Debug.Trace (trace)
import Data.Maybe
import Data.Bits
import Data.List (intercalate)
import AST

eval :: [(String, AST)] -> AST -> AST

-- Value
eval _ v@(Int     _) = v
eval _ v@(Real    _) = v
eval _ v@(String  _) = v
eval _ v@(Bool    _) = v
eval s v@(List   xs) = List $ map (eval s) xs
eval _ v@(Struct  _) = v
eval _ v@(Func _  _) = v
eval _ v@(Error   _) = v

-- Apply
eval s (Apply names args) = ref s names
  where
    ref :: [(String, AST)] -> [String] -> AST
    ref scope [] = Error "BUG1"
    ref scope (x:[]) = case find x scope of
        Func names' ast -> eval ((zip names' args') ++ scope) ast
        Struct fields   -> if length args == 0 then Struct fields else Struct $ zip (map fst fields) args'
        other           -> eval scope other
    ref scope (x:xs) = case find x scope of
        Struct fields -> ref (fields ++ scope) xs
        other         -> Error $ "BUG2 " ++ show other
    find x scope = case lookup x scope of
        Nothing  -> Error $ "not found " ++ (show x) ++ " scope=" ++ (format "," $ map fst scope)
        Just hit -> eval scope hit
    args' = map (eval s) args

-- Op
eval s (Op _ (Error a) (Error b)) = Error $ a ++ " : " ++ b
eval s (Op _ v@(Error _) _) = v 
eval s (Op _ _ v@(Error _)) = v
eval s (Op op l r) = binaryOp op (toValue l) (toValue r)
  where
    toValue x = case eval s x of
        v@(Int _) -> v
        v@(Real _) -> v
        v@(String _) -> v
        v@(Bool _) -> v
        v@(List _) -> v
        v@(Struct _) -> v
        v@(Error m) -> v
        v -> Error $ "not value" ++ (show v)
    binaryOp :: String -> AST -> AST -> AST
    -- Op Int
    binaryOp "+" (Int a) (Int b) = Int $ a + b
    binaryOp "-" (Int a) (Int b) = Int $ a - b
    binaryOp "*" (Int a) (Int b) = Int $ a * b
    binaryOp "//" (Int a) (Int b) = Int $ truncate $ a' / b'
      where
        a' = fromIntegral a
        b' = fromIntegral b
    binaryOp "%" (Int a) (Int b) = Int $ a `mod` b
    binaryOp "|" (Int a) (Int b) = Int $ a .|. b
    binaryOp "&" (Int a) (Int b) = Int $ a .&. b
    binaryOp "**" (Int a) (Int b) = Int $ a ^ b
    binaryOp "<<" (Int a) (Int b) = Int $ shiftL a (fromIntegral b :: Int)
    binaryOp ">>" (Int a) (Int b) = Int $ shiftR a (fromIntegral b :: Int)
    binaryOp "==" (Int a) (Int b) = Bool $ a == b
    -- Op Real
    binaryOp "+" (Real a) (Real b) = Real $ a + b
    binaryOp "-" (Real a) (Real b) = Real $ a - b
    binaryOp "*" (Real a) (Real b) = Real $ a * b
    binaryOp "/" (Real a) (Real b) = Real $ a / b
    binaryOp "**" (Real a) (Real b) = Real $ a ** b
    binaryOp "==" (Real a) (Real b) = Bool $ a == b
    -- Op String
    binaryOp "+" (String a) (String b) = String $ a ++ b
    binaryOp "==" (String a) (String b) = Bool $ a == b
    -- Op Bool
    binaryOp "&" (Bool a) (Bool b) = Bool $ and [a, b]
    binaryOp "|" (Bool a) (Bool b) = Bool $ or [a, b]
    binaryOp "==" (Bool a) (Bool b) = Bool $ a == b
    -- Op List
    binaryOp "+" (List a) (List b) =  List $ a ++ b
    binaryOp "==" (List a) (List b) = Bool $ a == b
    -- Op Struct
    binaryOp "+" (Struct a) (Struct b) =  Struct $ a ++ b
    binaryOp "==" (Struct a) (Struct b) = Bool $ a == b
    -- Op Invalid
    binaryOp op a b = Error $ "invalid operator (" ++ (show a) ++ ") " ++ op ++ " (" ++ (show b) ++ ")"

-- If
eval s (If cond a b) = case eval s cond of
    (Bool True)  -> eval s a
    (Bool False) -> eval s b
    _            -> Error $ "cond is not boolean " ++ show cond

-- utility
format glue [] = ""
format glue (x:xs) = x ++ glue ++ format glue xs
