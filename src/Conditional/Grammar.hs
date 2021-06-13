{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Conditional.Grammar where

import Test.QuickCheck
    ( Arbitrary(arbitrary), elements, frequency, oneof, sized, Gen )

import qualified Test.SmallCheck.Series as SC

-- ADT for result values
data Val = VInt Int | VBool Bool | VClos String Expr Environment
  deriving ( Eq )

-- ADT for type values
data Type = TInt | TBool | TClos Type Type
  deriving ( Eq )

-- Used for pretty printing types
instance Show Type where
  show TInt  = "Int"
  show TBool = "Bool"
  show (TClos argTy retTy) = show argTy ++ " -> " ++ show retTy 

-- Used for pretty printing result values
instance Show Val where
    show (VInt x)  = show x
    show (VBool x) = show x
    show (VClos arg body env) = "\\" ++ arg ++ ".(" ++ show body ++ ")"

-- Aliases for both environments
type Environment  = [(String, Val)]
type TEnvironment = [(String, Type)]

-- ADT for expressions which also contains conditionals
data Expr =
  -- basic building blocks
  EInt Int | EBool Bool | Id String
  -- basic operations on ints
  | Add (Expr, Expr) | Mul (Expr, Expr)
  -- basic operations on booleans
  | Not Expr | Or (Expr, Expr) | And (Expr, Expr)
  -- comparisons
  | Eq (Expr, Expr) | Lt (Expr, Expr) | Gt (Expr, Expr)
  -- functions
  | Lambda ((String, Type), Expr) | App (Expr, Expr)
  -- conditionals
  | If (Expr, (Expr, Expr))

  deriving ( Eq )

-- Pretty printing expressions
instance Show Expr where
  show (EInt x)            = if x < 0 then "(" ++ show x ++ ")" else show x
  show (EBool b)           = show b
  show (Id s)              = s
  show (Add (left, right)) = "(" ++ show left ++ " + " ++ show right ++ ")"
  show (Mul (left, right)) = "(" ++ show left ++ " * " ++ show right ++ ")"
  show (Not e)             = "(not " ++ show e ++ ")"
  show (Or (left, right))  = "(" ++ show left ++ " || " ++ show right ++ ")"
  show (And (left, right)) = "(" ++ show left ++ " && " ++ show right ++ ")"
  show (Eq (left, right))  = "(" ++ show left ++ " == " ++ show right ++ ")"
  show (Lt (left, right))  = "(" ++ show left ++ " < " ++ show right ++ ")"
  show (Gt (left, right))  = "(" ++ show left ++ " > " ++ show right ++ ")"
  show (Lambda (s, e))     = "(\\ (" ++ fst s ++ ": " ++ show (snd s) ++ ") " ++ show e ++ ")"
  show (App (f, e))        = "(" ++ show f ++ " " ++ show e ++ ")"
  show (If (i, (t, e)))    = "(if " ++ show i ++ " then " ++ show t ++ " else " ++ show e ++ ")"

-- Needed for SmallCheck enumerating
instance (Monad m) => SC.Serial m Expr where
  series = SC.cons1 EInt SC.\/ SC.cons1 EBool -- SC.\/ SC.cons1 Id
           SC.\/ SC.cons1 Add SC.\/ SC.cons1 Mul
           SC.\/ SC.cons1 Not SC.\/ SC.cons1 Or SC.\/ SC.cons1 And
           SC.\/ SC.cons1 Eq  SC.\/ SC.cons1 Lt SC.\/ SC.cons1 Gt
           -- SC.\/ SC.cons1 Lambda SC.\/ SC.cons1 App
           SC.\/ SC.cons1 If
