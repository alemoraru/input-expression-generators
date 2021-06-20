{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Arithmetic.Grammar where

import Test.SmallCheck.Series ( (\/), cons1, Serial(..) )

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
instance (Monad m) => Serial m Expr where
  series = cons1 Val \/ cons1 Add \/ cons1 Sub \/ cons1 Mul \/ cons1 Div