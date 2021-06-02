{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Booleans.Grammar ( Expr ( .. ) ) where

import Test.QuickCheck
import qualified Test.SmallCheck.Series as SC

import Control.Monad

-- Boolean ADT
data Expr = Val Bool
  | And Expr Expr
  | Or Expr Expr
  | Not Expr 
  deriving ( Eq )

-- Used for custom printing
instance Show Expr where
  show (Val x) = show x
  show (And left right) = "(" ++ show left ++ " && " ++ show right ++ ")"
  show (Or left right)  = "(" ++ show left ++ " || " ++ show right ++ ")"
  show (Not expr)       = "(!" ++ show expr ++ ")"
 
-- Necessary for QuickCheck random sampling
instance Arbitrary Expr where  
  arbitrary = sized arbExpr   

-- Function for generating data 
-- of a particular depth 
arbExpr :: Int -> Gen Expr
arbExpr 0 = Val <$> arbitrary  
arbExpr n = frequency 
  [
    (1, Val <$> arbitrary)
  , (2, liftM2 And (arbExpr (n `div` 2))
                   (arbExpr (n `div` 2)))  
  , (2, liftM2 Or (arbExpr (n `div` 2))
                  (arbExpr (n `div` 2)))
  , (2, fmap Not (arbExpr (n `div` 2))) 
  ]

-- Necessary for SmallCheck exhaustive generation
instance (Monad m) => SC.Serial m Expr where
  series = SC.cons1 Val SC.\/ SC.cons2 And SC.\/ SC.cons2 Or SC.\/ SC.cons1 Not