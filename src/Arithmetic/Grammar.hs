module Arithmetic.Grammar ( Expr ( .. ) ) where

import Test.QuickCheck
import Control.Monad

data Expr = Val Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving ( Eq )
  
instance Show Expr where
  show (Val x) = show x
  show (Add left right) = show left ++ " + " ++ show right
  show (Sub left right) = show left ++ " - " ++ show right
  show (Mul left right) = show left ++ " * " ++ show right
  show (Div left right) = show left ++ " / " ++ show right

instance Arbitrary Expr where  
  arbitrary = sized arbExpr 

arbExpr :: Int -> Gen Expr
arbExpr 0 = undefined 
arbExpr n = undefined 
