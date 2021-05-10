module Booleans.Grammar ( Expr ( .. ) ) where

import Test.QuickCheck

import Control.Monad

data Expr = Val Bool
  | And Expr Expr
  | Or Expr Expr
  | Not Expr 
  deriving ( Eq )
  
instance Show Expr where
  show (Val x) = show x
  show (And left right) = "(" ++ show left ++ " && " ++ show right ++ ")"
  show (Or left right)  = "(" ++ show left ++ " || " ++ show right ++ ")"
  show (Not expr)       = "(!" ++ show expr ++ ")"
 
instance Arbitrary Expr where  
  arbitrary = sized arbExpr   

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