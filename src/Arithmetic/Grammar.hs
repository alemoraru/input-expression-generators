{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Arithmetic.Grammar where

import Test.QuickCheck
    ( Arbitrary(arbitrary), frequency, sized, Gen )
import qualified Test.SmallCheck.Series as SC

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

-- Necessary for QuickCheck random sampling
instance Arbitrary Expr where
  arbitrary = sized arbExpr

-- Function for generating data 
-- of a particular depth 
arbExpr :: Int -> Gen Expr
arbExpr 0 = fmap Val arbitrary
arbExpr n = frequency
  [ (1, fmap Val arbitrary)
  , (2, do
          left  <- arbExpr (n `div` 2)
          right <- arbExpr (n `div` 2)
          return $ Add (left, right))
  , (2, do
          left  <- arbExpr (n `div` 2)
          right <- arbExpr (n `div` 2)
          return $ Sub (left, right))
  , (2, do
          left  <- arbExpr (n `div` 2)
          right <- arbExpr (n `div` 2)
          return $ Mul (left, right))
  , (2, do
          left  <- arbExpr (n `div` 2)
          right <- arbExpr (n `div` 2)
          return $ Div (left, right))
  ]

-- Necessary for SmallCheck exhaustive generation
instance (Monad m) => SC.Serial m Expr where
  series = SC.cons1 Val SC.\/ SC.cons1 Add SC.\/ SC.cons1 Sub SC.\/ SC.cons1 Mul SC.\/ SC.cons1 Div