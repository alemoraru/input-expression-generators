{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Booleans.Grammar ( Expr ( .. ) ) where

import Test.SmallCheck.Series ( (\/), cons1, Serial(..) )

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
instance (Monad m) => Serial m Expr where
  series = cons1 Val \/ cons1 And \/ cons1 Or \/ cons1 Not