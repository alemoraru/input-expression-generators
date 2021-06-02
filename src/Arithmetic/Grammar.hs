{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Arithmetic.Grammar ( Expr ( .. ) ) where

import Test.QuickCheck
import qualified Test.SmallCheck.Series as SC

import Control.Monad

-- Arithmetic ADT
data Expr = Val Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving ( Eq )
  
-- Needed for printing
instance Show Expr where
  show (Val x) = if x < 0 then "(" ++ show x ++ ")" else show x
  show (Add left right) = show left ++ " + " ++ show right
  show (Sub left right) = show left ++ " - " ++ show right
  show (Mul left right) = show left ++ " * " ++ show right
  show (Div left right) = show left ++ " / " ++ show right

-- Necessary for QuickCheck random sampling
instance Arbitrary Expr where  
  arbitrary = sized arbExpr   

-- Function for generating data 
-- of a particular depth 
arbExpr :: Int -> Gen Expr
arbExpr 0 = fmap Val arbitrary  
arbExpr n = frequency 
  [ (1, fmap Val arbitrary)
  , (2, liftM2 Add (arbExpr (n `div` 2))
                   (arbExpr (n `div` 2)))
  , (2, liftM2 Sub (arbExpr (n `div` 2))
                   (arbExpr (n `div` 2)))
  , (2, liftM2 Mul (arbExpr (n `div` 2))
                   (arbExpr (n `div` 2)))
  , (2, liftM2 Div (arbExpr (n `div` 2))
                   (arbExpr (n `div` 2)))
  ] 

-- Necessary for SmallCheck exhaustive generation
instance (Monad m) => SC.Serial m Expr where
  series = SC.cons1 Val SC.\/ SC.cons2 Add SC.\/ SC.cons2 Sub SC.\/ SC.cons2 Mul SC.\/ SC.cons2 Div