module Eval (eval) where

import           AST
import           Data.Bits
import           Data.List (intercalate)
import           Data.Maybe
import           Data.Char (isAlpha)
import           Debug.Trace (trace)

eval :: [(String, AST)] -> AST -> AST

-- Value
eval _ v@(Char      _)  = v
eval _ v@(Int       _)  = v
eval _ v@(Real      _)  = v
eval s v@(List     xs)  = List $ map (eval s) xs
eval _ v@(Bool      _)  = v
eval _ v@(String    _)  = v
eval _ v@(Func [] ast)  = ast
eval _ v@(Func    _ _)  = v
eval _ v@(Struct    _)  = v
eval _ v@(Recursive _)  = v
eval s v@(Instance name fields) = Instance name (map (\(field, ast) -> (field, eval s ast)) fields)
eval _ v@(Assign  _ _)  = v
eval _ v@(Error     _)  = v

-- Ref
eval s v@(Ref (first:relatives)) = case lookup first s of
    Nothing  -> notfound "Ref1" first s
    Just hit -> eval s $ go hit relatives
  where
    go acc [] = eval s acc
    go (Recursive []) names = notfound "Ref2" (show names) s
    go (Recursive ((name1:fields):rest)) [name2] = if name1 == name2
      then Func fields $ Instance name1 (map (\name -> (name, Ref [name])) fields)
      else go (Recursive rest) [name2]
    go (Struct fields) (x:xs) = case lookup x fields of
      Just hit -> go hit xs
      Nothing -> notfound "Ref3 " x s
    go acc rest = notfound "Ref4" (show rest) s

-- Seq
eval s (Seq []) = Error "empty sequence"
eval s (Seq all) = go s all
  where
    go s [x] = eval s x
    go s ((Assign name ast):xs) = go ((name, ast) : s) xs
    go s (x:xs) = eval s (Call [Ref ["bind"], eval s x, go s xs])

-- Call
eval s term@(Call all@(first:args)) = case first of
    Ref [name] -> if isBuildin name then buildin name (map (eval s) args) else call (map (eval s) all)
    _ -> call (map (eval s) all)
  where
    buildin "if" [cond, a, b] = case eval s cond of
      (Bool True)  -> eval s a
      (Bool False) -> eval s b
      _            -> Error $ "cond is not boolean " ++ show cond
    buildin op [l] = singleOp op (eval s l)
    buildin op [l, r] = binaryOp op (eval s l) (eval s r)
    buildin name args = Error $ "buildin " ++ name ++ " " ++ show args
    -- lookup user defination
    call ((Func names ast):args) = eval ((zip names args) ++ s) ast
    call ((Struct fields):args) = Struct $ map (\((name, _), arg) -> (name, arg)) $ zip fields args
    call ((Ref name):[Struct fields]) = eval (fields ++ s) (Ref name)
    -- otherwise bug
    call rest = Error $ "call " ++ show rest
    -- helpers
    singleOp "+" l = l
    singleOp op l = Error $ "unknown single op " ++ op ++ " " ++ (show l) ++ "\n" ++ (show term)
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
notfound mark name s = Error $ mark ++ " not found '" ++ name ++ "' scope =" ++ (join "" $ map fst s)
join acc [] = acc
join acc (x:xs) = join (acc ++ " " ++ x) xs
isBuildin [] = False
isBuildin "if" = True
isBuildin (x:_) = not $ isAlpha x
