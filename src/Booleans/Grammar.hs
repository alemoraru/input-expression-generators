module Booleans.Grammar ( Expr ( .. ) ) where

import Test.QuickCheck
import Control.Monad

data Expr = Box Bool
  | And Expr Expr
  | Or Expr Expr
  | Not Expr 
  deriving ( Eq )
  
instance Show Expr where
  show (Box x) = show x
  show (And left right) = show left ++ " + " ++ show right
  show (Or left right)  = show left ++ " - " ++ show right
  show (Not expr)       = show expr

instance Arbitrary Expr where  
  arbitrary = sized arbExpr   

arbExpr :: Int -> Gen Expr
arbExpr 0 = undefined 
arbExpr n = undefined 