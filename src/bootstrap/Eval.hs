module Eval (eval) where

import           AST
import           Data.Bits
import           Data.List   (intercalate)
import           Data.Maybe
import           Debug.Trace (trace)

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

-- Ref
eval s (Ref x) = case lookup x s of
  Nothing  -> Error $ " not found '" ++ x ++ "' scope=" ++ (show s)
  Just hit -> eval s hit

-- Call
eval s (Call (x:xs)) = go x (map (eval s) xs)
  where
    go (Func args1 ast) args2 = eval (zip args1 args2) ast
    go (Struct fields) args = Struct $ map (\((name, _), arg) -> (name, arg)) $ zip fields args
    go (Ref name) [Struct fields] = eval (fields ++ s) (Ref name)
    -- if
    go (Ref "if") [cond, a, b] = case eval s cond of
      (Bool True)  -> eval s a
      (Bool False) -> eval s b
      _            -> Error $ "cond is not boolean " ++ show cond
    -- Op
    go (Ref op) [l] = singleOp op (eval s l)
    go (Ref op) [l, r] = binaryOp op (eval s l) (eval s r)
    -- bug
    go x xs = Error $ "bug " ++ show x ++ show xs
    -- Single op
    singleOp "+" l = l
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
    -- Op Bool
    binaryOp "==" (Struct a) (Struct b) = Bool $ a == b
    -- Op Invalid
    binaryOp op a b = Error $ "invalid operator (" ++ (show a) ++ ") " ++ op ++ " (" ++ (show b) ++ ")"

-- utility
format glue [] = ""
format glue (x:xs) = x ++ glue ++ format glue xs
