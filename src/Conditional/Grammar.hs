{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Conditional.Grammar where

import Test.SmallCheck.Series

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
  EInt Int | EBool Bool | Id String
  | Add (Expr, Expr) | Mul (Expr, Expr)
  | Not Expr | Or (Expr, Expr) | And (Expr, Expr)
  | Eq (Expr, Expr) | Lt (Expr, Expr) | Gt (Expr, Expr)
  | Lambda ((String, Type), Expr) | App (Expr, Expr)
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
instance (Monad m) => Serial m Expr where
  series = cons1 EInt \/ cons1 EBool -- \/ cons1 Id
           \/ cons1 Add \/ cons1 Mul
           \/ cons1 Not \/ cons1 Or \/ cons1 And
           \/ cons1 Eq  \/ cons1 Lt \/ cons1 Gt
           -- \/ cons1 Lambda \/ cons1 App
           \/ cons1 If
