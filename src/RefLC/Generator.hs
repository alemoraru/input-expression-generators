module RefLC.Generator where

import RefLC.Grammar ( spTerm, Expr, Type(TInt, TFun), sampleTEnv )
import RefLC.TypeChecker ( typeCheck )
import Spaces ( uniform, uniformFilter )

import qualified Test.QuickCheck as QC

-- Predicate that checks if a given term is of a specified type
predicate :: Expr -> Bool
predicate = typeCheck sampleTEnv (TInt) 

-- Get a random lambda term 
getTerm :: IO Expr 
getTerm = QC.generate $ uniformFilter predicate spTerm 7

getTermFaster :: IO Expr 
getTermFaster = QC.generate $ uniform predicate spTerm 8

-- Auxiliary function for QuickCheck arbitrary function
arbExpr :: QC.Gen Expr 
arbExpr = uniformFilter predicate spTerm 7

-- Useful for QuickCheck properties
instance QC.Arbitrary Expr where
    arbitrary = arbExpr
