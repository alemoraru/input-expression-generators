module Arithmetic.Grammar ( Expr ( .. ) ) where

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
