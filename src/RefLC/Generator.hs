module RefLC.Generator where

import RefLC.Grammar ( spTerm, Expr, Type(TInt, TFun), sampleTEnv )
import RefLC.TypeChecker ( typeCheck )
import Spaces ( uniform, uniformFilter )

import Test.QuickCheck ( Gen, generate, Arbitrary(arbitrary) )

-- Predicate that checks if a given term is of a specified type
predicate :: Expr -> Bool
predicate = typeCheck sampleTEnv (TFun (TInt , TInt)) 

-- Get a random lambda term 
getTerm :: IO Expr 
getTerm = generate $ uniformFilter predicate spTerm 7

-- Get a random lambda term but faster
getTermFaster :: IO Expr 
getTermFaster = generate $ uniform predicate spTerm 12

-- Auxiliary function for QuickCheck arbitrary function
arbExpr :: Gen Expr 
arbExpr = uniform predicate spTerm 7

-- Useful for QuickCheck properties
instance Arbitrary Expr where
    arbitrary = arbExpr
