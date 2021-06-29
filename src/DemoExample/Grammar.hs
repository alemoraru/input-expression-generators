{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DemoExample.Grammar where

import Spaces
    ( Space((:+:), (:*:), Pay, Pure, (:$:)), uniformFilter )
import Test.SmallCheck.Series ( (\/), cons1, Serial(..) )
import qualified Test.QuickCheck as QC

-- ADT example
data Expr = Num Int | Add (Expr, Expr) 

instance Show Expr where
  show (Num i)        = if i >= 0 then show i else "(" ++ show i ++ ")" 
  show (Add (e1 ,e2)) = show e1 ++ " + " ++ show e2

-- Correct interpreter
eval :: Expr -> Int 
eval (Num i)        = i
eval (Add (e1, e2)) = eval e1 + eval e2

-- Faulty intepreter
evil :: Expr -> Int 
evil (Num i)        = i
evil (Add (e1, e2)) = eval e1 + eval e1

-- Space of expressions
spExpr, spNum, spAdd :: Space Expr 
spExpr = Pay (spAdd :+: spNum)
spAdd  = Add :$: (spExpr :*: spExpr)
spNum  = Num :$: spInt

-- Space of int values (starts enumerating from 0)
spInt :: Space Int
spInt = Pay (Pure 0 :+: (succ :$: spInt))

-- Necessary for SmallCheck exhaustive generation
instance (Monad m) => Serial m Expr where
  series = cons1 Num \/ cons1 Add

-- Auxiliary function for QuickCheck arbitrary function
-- Uses a universally true predicate (no need for type-checking)
arbUniformExpr :: QC.Gen Expr
arbUniformExpr = uniformFilter (const True) spExpr 9

-- Function for generating data 
-- of a particular depth 
arbNaiveExpr :: Int -> QC.Gen Expr
arbNaiveExpr 0 = fmap Num QC.arbitrary
arbNaiveExpr n = QC.frequency
  [ 
    (1, fmap Num QC.arbitrary)
  , (2, do
          left  <- arbNaiveExpr (n `div` 2)
          right <- arbNaiveExpr (n `div` 2)
          return $ Add (left, right)
    )
  ]

-- Needed for QuickCheck properties
instance QC.Arbitrary Expr where
    arbitrary = QC.frequency 
      [
        (1, QC.sized arbNaiveExpr), 
        (0, arbUniformExpr)
      ]