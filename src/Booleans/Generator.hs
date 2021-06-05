module Booleans.Generator where

import Spaces.Definition
    ( Space((:+:), (:*:), (:$:), Pay, Pure), uniformFilter )

import Test.QuickCheck ( Arbitrary(arbitrary), Gen )

-- Boolean ADT
data Expr = Val Bool
  | And (Expr, Expr)
  | Or  (Expr, Expr)
  | Not Expr
  deriving ( Eq )

-- Used for custom printing
instance Show Expr where
  show (Val x)             = show x
  show (And (left, right)) = "(" ++ show left ++ " && " ++ show right ++ ")"
  show (Or (left, right))  = "(" ++ show left ++ " || " ++ show right ++ ")"
  show (Not expr)          = "(!" ++ show expr ++ ")"

-- Spaces for the needed boolean constructors 
spExpr, spAnd, spOr, spNot, spVal :: Space Expr
spExpr = Pay (spAnd :+: spOr :+: spNot :+: spVal)
spAnd  = And :$: (spExpr :*: spExpr)
spOr   = Or :$: (spExpr :*: spExpr)
spNot  = Not :$: spExpr
spVal  = Val :$: spBool

-- Space of boolean values (just True & False values)
spBool :: Space Bool
spBool = Pay (Pure True :+: Pure False)

-- Auxiliary function for QuickCheck arbitrary function
-- Uses a universally true predicate (no need for type-checking)
arbExpr :: Gen Expr
arbExpr = uniformFilter (const True) spExpr 7

-- Useful for QuickCheck properties
instance Arbitrary Expr where
    arbitrary = arbExpr