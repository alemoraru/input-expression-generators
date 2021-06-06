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

-- Correct interpreter
interpC1 :: Expr -> Bool
interpC1 (Val x)             = x
interpC1 (And (left, right)) = interpC1 left && interpC1 right
interpC1 (Or (left, right))  = interpC1 left || interpC1 right
interpC1 (Not expr)          = not $ interpC1 expr

-- Correct interpreter
interpC2 :: Expr -> Bool
interpC2 (Val x)             = x
interpC2 (And (left, right)) = interpC2 right && interpC2 left
interpC2 (Or (left, right))  = interpC2 right || interpC2 left
interpC2 (Not expr)          = not $ interpC2 expr

-- Incorrect interpreter
interpF1 :: Expr -> Bool
interpF1 (Val x)             = x
interpF1 (And (left, right)) = interpF1 left || interpF1 right -- introduced flaw here
interpF1 (Or (left, right))  = interpF1 left || interpF1 right
interpF1 (Not expr)          = not $ interpF1 expr

-- Incorrect interpreter
interpF2 :: Expr -> Bool
interpF2 (Val x)             = x
interpF2 (And (left, right)) = interpF2 left && interpF2 right
interpF2 (Or (left, right))  = interpF2 left && interpF2 right -- introduced flaw here
interpF2 (Not expr)          = not $ interpF2 expr

-- Incorrect interpreter
interpF3 :: Expr -> Bool
interpF3 (Val x)             = x
interpF3 (And (left, right)) = interpF3 left && interpF3 right
interpF3 (Or (left, right))  = interpF3 left || interpF3 right
interpF3 (Not expr)          = interpF3 expr -- introduced flaw here