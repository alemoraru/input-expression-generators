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