module Arithmetic.Generator where

import Spaces.Definition
    ( Space((:+:), (:*:), Pay, Pure, (:$:)), uniformFilter )

import Test.QuickCheck ( Arbitrary(arbitrary), Gen )

-- Arithmetic ADT
data Expr = Val Int
  | Add (Expr, Expr)
  | Sub (Expr, Expr)
  | Mul (Expr, Expr)
  | Div (Expr, Expr)
  deriving ( Eq )

-- Needed for printing
instance Show Expr where
  show (Val x) = if x < 0 then "(" ++ show x ++ ")" else show x
  show (Add (left, right)) = show left ++ " + " ++ show right
  show (Sub (left, right)) = show left ++ " - " ++ show right
  show (Mul (left, right)) = show left ++ " * " ++ show right
  show (Div (left, right)) = show left ++ " / " ++ show right

-- Spaces for the needed boolean constructors 
spExpr, spAdd, spSub, spMul, spDiv, spVal :: Space Expr
spExpr = Pay (spAdd :+: spSub :+: spMul :+: spDiv :+: spVal)
spAdd  = Add :$: (spExpr :*: spExpr)
spSub  = Sub :$: (spExpr :*: spExpr)
spMul  = Mul :$: (spExpr :*: spExpr)
spDiv  = Div :$: (spExpr :*: spExpr)
spVal  = Val :$: spInt

-- Space of int values (starts from 1 because of issues with zero)
spInt :: Space Int
spInt = Pay (Pure 1 :+: (succ :$: spInt))

-- Auxiliary function for QuickCheck arbitrary function
-- Uses a universally true predicate (no need for type-checking)
arbExpr :: Gen Expr
arbExpr = uniformFilter (const True) spExpr 6

-- Useful for QuickCheck properties
instance Arbitrary Expr where
    arbitrary = arbExpr

-- Correct interpreter for arithmetic expressions
interpC1 :: Expr -> Int
interpC1 (Val x) = x
interpC1 (Add (left, right)) = interpC1 left + interpC1 right
interpC1 (Sub (left, right)) = interpC1 left - interpC1 right
interpC1 (Mul (left, right)) = interpC1 left * interpC1 right
interpC1 (Div (left, right)) = interpC1 left `div` interpC1 right

-- Correct interpreter for arithmetic expressions
interpC2 :: Expr -> Int
interpC2 (Val x) = x
interpC2 (Add (left, right)) = interpC2 right + interpC2 left
interpC2 (Sub (left, right)) = interpC2 left - interpC2 right
interpC2 (Mul (left, right)) = interpC2 right * interpC2 left
interpC2 (Div (left, right)) = interpC2 left `div` interpC2 right

-- Faulty interpreter for arithmetic expressions
interpF1 :: Expr -> Int
interpF1 (Val x) = x
interpF1 (Add (left, right)) = interpF1 left + interpF1 left -- introduced flaw here
interpF1 (Sub (left, right)) = interpF1 left - interpF1 right
interpF1 (Mul (left, right)) = interpF1 left * interpF1 right
interpF1 (Div (left, right)) = interpF1 left `div` interpF1 right

-- Faulty interpreter for arithmetic expressions
interpF2 :: Expr -> Int
interpF2 (Val x) = x
interpF2 (Add (left, right)) = interpF2 left + interpF2 right 
interpF2 (Sub (left, right)) = interpF2 left - interpF2 left -- introduced flaw here
interpF2 (Mul (left, right)) = interpF2 left * interpF2 right
interpF2 (Div (left, right)) = interpF2 left `div` interpF2 right

-- Faulty interpreter for arithmetic expressions
interpF3 :: Expr -> Int
interpF3 (Val x) = x
interpF3 (Add (left, right)) = interpF3 left + interpF3 right 
interpF3 (Sub (left, right)) = interpF3 left - interpF3 right 
interpF3 (Mul (left, right)) = interpF3 left * interpF3 left -- introduced flaw here
interpF3 (Div (left, right)) = interpF3 left `div` interpF3 right

-- Faulty interpreter for arithmetic expressions
interpF4 :: Expr -> Int
interpF4 (Val x) = x
interpF4 (Add (left, right)) = interpF4 left + interpF4 right 
interpF4 (Sub (left, right)) = interpF4 left - interpF4 right 
interpF4 (Mul (left, right)) = interpF4 left * interpF4 right
interpF4 (Div (left, right)) = interpF4 left `div` interpF4 left -- introduced flaw here