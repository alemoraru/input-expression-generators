module Spaces.Generator where

import Spaces.Grammar
import Spaces.Definition
import Spaces.Interp

import qualified Test.QuickCheck as QC

-- Predicate that checks if a given term is of a specified type
predicate :: Expr -> Bool
predicate = typeCheck sampleTEnv (TInt) 

-- Get a random lambda term 
getLambdaTerm :: IO Expr 
getLambdaTerm = QC.generate $ uniformFilter predicate spTerm 8

-- Auxiliary function for QuickCheck arbitrary function
arbExpr :: QC.Gen Expr 
arbExpr = uniformFilter predicate spTerm 8

-- Useful for QuickCheck properties
instance QC.Arbitrary Expr where
    arbitrary = arbExpr
