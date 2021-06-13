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

-- Needed for pretty-printing
instance Show Expr where
  show (Val x) = if x < 0 then "(" ++ show x ++ ")" else show x
  show (Add (left, right)) = show left ++ " + " ++ show right
  show (Sub (left, right)) = show left ++ " - " ++ show right
  show (Mul (left, right)) = show left ++ " * " ++ show right
  show (Div (left, right)) = show left ++ " / " ++ show right

-- Necessary for SmallCheck exhaustive generation
instance (Monad m) => SC.Serial m Expr where
  series = SC.cons1 Val SC.\/ SC.cons1 Add SC.\/ SC.cons1 Sub SC.\/ SC.cons1 Mul SC.\/ SC.cons1 Div