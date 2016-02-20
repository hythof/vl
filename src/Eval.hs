module Eval where

import Debug.Trace
import Data.Maybe
import Data.Bits
import Data.List (intercalate)
import AST

eval :: [(String, AST)] -> AST -> AST

-- Value
eval _ v@(Int    _) = v
eval _ v@(Float  _) = v
eval _ v@(String _) = v
eval _ v@(Bool   _) = v
eval _ v@(List   _) = v
eval _ v@(Struct _) = v
eval _ v@(Func _ _) = v
eval _ v@(Error  _) = v

-- Lookup
eval s (Lookup names arguments) = single s names
  where
    apply scope (Func args ast) = eval (newScope args) ast
    apply scope (Struct fields) = Struct $ zipWith (\(name, _) new -> (name, new)) fields params
    apply scope x = eval scope x
    newScope args = zip args params
    params = map (eval s) arguments
    single :: [(String, AST)] -> [String] -> AST
    single scope (target:rest) =
        if length rest == 0 then
            if length arguments == 0 then
                eval scope hit
            else
                apply scope hit
        else
            single (newScope hit) rest
      where
        newScope (Struct xs) = xs ++ scope
        newScope x = trace ("\nBUG " ++ show x) scope
        hits = map (\(_, x) -> x) $ filter (\(x, _) -> target == x) scope
        hit = if length hits > 0 then
                  eval scope $ head hits
              else
                  Error $ "not found '" ++ target ++ "'"
                          ++ " at " ++ (intercalate "." names) 
                          ++ " in " ++ (show scope)

-- Op
eval s (Op _ (Error a) (Error b)) = Error $ a ++ " : " ++ b
eval s (Op _ v@(Error _) _) = v 
eval s (Op _ _ v@(Error _)) = v
eval s (Op op l r) = binaryOp op (toValue l) (toValue r)
  where
    toValue x =
      case eval s x of
        v@(Int _) -> v
        v@(Float _) -> v
        v@(String _) -> v
        v@(Bool _) -> v
        v@(List _) -> v
        v@(Struct _) -> v
        v@(Error m) -> Error $ "not value " ++ m
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
    -- Op Float
    binaryOp "+" (Float a) (Float b) = Float $ a + b
    binaryOp "-" (Float a) (Float b) = Float $ a - b
    binaryOp "*" (Float a) (Float b) = Float $ a * b
    binaryOp "/" (Float a) (Float b) = Float $ a / b
    binaryOp "**" (Float a) (Float b) = Float $ a ** b
    binaryOp "==" (Float a) (Float b) = Bool $ a == b
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
    binaryOp op a b = Error $ "invalid operator " ++ op ++ " : " ++ (show a) ++ " " ++ (show b)

-- If
eval s (If cond t f) = case eval s cond of
    (Bool True)  -> eval s t
    (Bool False) -> eval s f
    _            -> Error $ "cond is not boolean " ++ show cond
