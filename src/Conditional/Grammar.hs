{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Conditional.Grammar ( Expr ( .. ), Val ( .. ), Environment ) where

import Test.QuickCheck

import Control.Monad

import qualified Test.SmallCheck.Series as SC

data Val = VInt Int | VBool Bool | VClos String Expr Environment
  deriving ( Eq )

instance Show Val where
    show (VInt x)  = show x
    show (VBool x) = show x
    show (VClos arg body env) = "\\" ++ arg ++ ".(" ++ show body ++ ")"

type Environment = [(String, Val)]

data Expr =
  -- basic building blocks
  EInt Int | EBool Bool | Id String
  -- basic operations on ints
  | Add Expr Expr | Mul Expr Expr
  -- basic operations on booleans
  | Not Expr | Or Expr Expr | And Expr Expr
  -- comparisons
  | Eq Expr Expr | Lt Expr Expr | Gt Expr Expr
  -- functions
  | Lambda String Expr | App Expr Expr
  -- conditionals
  | If Expr Expr Expr

  deriving ( Eq )

instance Show Expr where
  show (EInt x)  = if x < 0 then "(" ++ show x ++ ")" else show x
  show (EBool b) = show b
  show (Id s)    = s

  show (Add left right) = "(" ++ show left ++ " + " ++ show right ++ ")"
  show (Mul left right) = "(" ++ show left ++ " * " ++ show right ++ ")"

  show (Not e)          = "(not " ++ show e ++ ")"
  show (Or left right)  = "(" ++ show left ++ " || " ++ show right ++ ")"
  show (And left right) = "(" ++ show left ++ " && " ++ show right ++ ")"

  show (Eq left right) = "(" ++ show left ++ " == " ++ show right ++ ")"
  show (Lt left right) = "(" ++ show left ++ " < " ++ show right ++ ")"
  show (Gt left right) = "(" ++ show left ++ " > " ++ show right ++ ")"

  show (Lambda s e) = "(\\ (" ++ s ++ ") " ++ show e ++ ")"
  show (App f e)    = "(" ++ show f ++ " " ++ show e ++ ")"

  show (If i t e) = "(if " ++ show i ++ " then " ++ show t ++ " else " ++ show e ++ ")"

-- Needed for SmallCheck enumerating
instance (Monad m) => SC.Serial m Expr where
  series = SC.cons1 EInt SC.\/ SC.cons1 EBool -- SC.\/ SC.cons1 Id
           SC.\/ SC.cons2 Add SC.\/ SC.cons2 Mul
           SC.\/ SC.cons1 Not SC.\/ SC.cons2 Or SC.\/ SC.cons2 And
           SC.\/ SC.cons2 Eq SC.\/ SC.cons2 Lt SC.\/ SC.cons2 Gt
           -- SC.\/ SC.cons2 Lambda SC.\/ SC.cons2 App
           SC.\/ SC.cons3 If

-- Needed for QuickCheck random sampling
instance Arbitrary Expr where
  arbitrary = sized arbExpr

arbExpr 0 = oneof [EInt <$> arbitrary, EBool <$> arbitrary]
arbExpr n = frequency
  [
    (1, oneof [EInt <$> arbitrary, EBool <$> arbitrary, arbVar sampleEnvironment]) -- Leaf generation (lowest prob to generate)
  , (4, liftM2 Add (arbExpr (n `div` 2))
                   (arbExpr (n `div` 2)))
  , (4, liftM2 Mul (arbExpr (n `div` 2))
                   (arbExpr (n `div` 2)))
  , (4, Not <$> arbExpr (n `div` 2))
  , (4, liftM2 Or (arbExpr (n `div` 2))
                  (arbExpr (n `div` 2)))
  , (4, liftM2 And (arbExpr (n `div` 2))
                   (arbExpr (n `div` 2)))
  , (4, liftM2 Eq (arbExpr (n `div` 2))
                  (arbExpr (n `div` 2)))
  , (4, liftM2 Lt (arbExpr (n `div` 2))
                  (arbExpr (n `div` 2)))
  , (4, liftM2 Gt (arbExpr (n `div` 2))
                  (arbExpr (n `div` 2)))
  , (4, liftM3 If (EBool <$> arbitrary)
                  (arbExpr (n `div` 2))
                  (arbExpr (n `div` 2)))
  ]

-- Get an arbitrary variable from the available environment
arbVar :: Environment -> Gen Expr
arbVar nv = 
  do 
    (key, val) <- elements nv
    return (Id key) 

sampleEnvironment :: Environment 
sampleEnvironment = 
  [
    ("zero", VInt 0),
    ("one", VInt 1),
    ("two", VInt 2),
    ("three", VInt 3),
    ("tru", VBool True),
    ("fls", VBool False)
  ]
