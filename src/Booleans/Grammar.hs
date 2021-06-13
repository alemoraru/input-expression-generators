{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Booleans.Grammar ( Expr ( .. ) ) where

import Test.QuickCheck
import qualified Test.SmallCheck.Series as SC

import Control.Monad

-- Boolean ADT
data Expr = Val Bool
  | And (Expr, Expr)
  | Or  (Expr, Expr)
  | Not Expr 
  deriving ( Eq )

-- Used for custom printing
instance Show Expr where
  show (Val x)             = show x
  show (And (left, right)) = "("  ++ show left ++ " && " ++ show right ++ ")"
  show (Or  (left, right)) = "("  ++ show left ++ " || " ++ show right ++ ")"
  show (Not expr)          = "(!" ++ show expr ++ ")"

-- Necessary for SmallCheck exhaustive generation
instance (Monad m) => SC.Serial m Expr where
  series = SC.cons1 Val SC.\/ SC.cons1 And SC.\/ SC.cons1 Or SC.\/ SC.cons1 Not