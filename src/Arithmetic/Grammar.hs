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
  show (Val x) = if x < 0 then "(" ++ show x ++ ")" else show x
  show (Add left right) = show left ++ " + " ++ show right
  show (Sub left right) = show left ++ " - " ++ show right
  show (Mul left right) = show left ++ " * " ++ show right
  show (Div left right) = show left ++ " / " ++ show right

instance Arbitrary Expr where  
  arbitrary = sized arbExpr   

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
