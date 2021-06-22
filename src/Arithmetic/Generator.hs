module Arithmetic.Generator where

import Arithmetic.Grammar ( Expr(..) ) 
import Spaces ( Space((:+:), (:*:), Pay, Pure, (:$:)), uniformFilter )
import Test.QuickCheck ( Arbitrary(arbitrary), Gen, frequency, chooseInt, sized )

-- Spaces for the needed boolean constructors 
spExpr, spAdd, spSub, spMul, spDiv, spVal :: Space Expr
spExpr = Pay (spAdd :+: spSub :+: spMul :+: spDiv :+: spVal)
spAdd  = Add :$: (spExpr :*: spExpr)
spSub  = Sub :$: (spExpr :*: spExpr)
spMul  = Mul :$: (spExpr :*: spExpr)
spDiv  = Div :$: (spExpr :*: spExpr)
spVal  = Val :$: spInt

-- Space of int values (starts enumerating from 0)
spInt :: Space Int
spInt = Pay (Pure 0 :+: (succ :$: spInt))

-- Auxiliary function for QuickCheck arbitrary function
-- Uses a universally true predicate (no need for type-checking)
arbUniformExpr :: Gen Expr
arbUniformExpr = uniformFilter (const True) spExpr 7

-- Function for generating data 
-- of a particular depth 
arbNaiveExpr :: Int -> Gen Expr
arbNaiveExpr 0 = fmap Val arbitrary
arbNaiveExpr n = frequency
  [ 
    (1, fmap Val arbitrary)
  , (2, do
          left  <- arbNaiveExpr (n `div` 2)
          right <- arbNaiveExpr (n `div` 2)
          return $ Add (left, right))
  , (2, do
          left  <- arbNaiveExpr (n `div` 2)
          right <- arbNaiveExpr (n `div` 2)
          return $ Sub (left, right))
  , (2, do
          left  <- arbNaiveExpr (n `div` 2)
          right <- arbNaiveExpr (n `div` 2)
          return $ Mul (left, right))
  , (2, do
          left  <- arbNaiveExpr (n `div` 2)
          right <- arbNaiveExpr (n `div` 2)
          return $ Div (left, right))
  ]

-- Needed for QuickCheck properties
instance Arbitrary Expr where
    arbitrary = frequency 
      [
        (1, sized arbNaiveExpr), 
        (0, arbUniformExpr)
      ]