module Booleans.Generator where

import Booleans.Grammar ( Expr(..) )
import Spaces ( Space((:+:), (:*:), (:$:), Pay, Pure), uniformFilter )
import Test.QuickCheck ( Arbitrary(arbitrary), Gen, frequency, sized )

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
arbUniformExpr :: Gen Expr
arbUniformExpr = uniformFilter (const True) spExpr 9

-- Function for generating data 
-- of a particular depth 
arbNaiveExpr :: Int -> Gen Expr
arbNaiveExpr 0 = Val <$> arbitrary  
arbNaiveExpr n = frequency 
  [
    (1, Val <$> arbitrary)
  , (2, do 
          left  <- arbNaiveExpr (n `div` 2)
          right <- arbNaiveExpr (n `div` 2)
          return $ And (left, right))  
  , (2, do 
          left  <- arbNaiveExpr (n `div` 2)
          right <- arbNaiveExpr (n `div` 2)
          return $ Or (left, right))
  , (2, fmap Not (arbNaiveExpr (n `div` 2))) 
  ]

-- Needed for QuickCheck properties
instance Arbitrary Expr where
    arbitrary = frequency [(1, sized arbNaiveExpr), (4, arbUniformExpr)]