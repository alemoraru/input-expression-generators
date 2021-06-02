module Spaces.Generator where

import Spaces.Grammar
import Spaces.Definition
import Spaces.Interp

import qualified Test.QuickCheck as QC

-- Predicate that checks if a given term is of a specified type
predicate :: Expr -> Bool
predicate = typeCheck sampleTEnv (TFun (TInt, TInt)) 

-- Get a random lambda term 
getTerm :: IO Expr 
getTerm = QC.generate $ uniformFilter predicate spTerm 7

getTermFaster :: IO Expr 
getTermFaster = QC.generate $ uniform predicate spTerm 7

-- Auxiliary function for QuickCheck arbitrary function
arbExpr :: QC.Gen Expr 
arbExpr = uniformFilter predicate spTerm 7

-- Useful for QuickCheck properties
instance QC.Arbitrary Expr where
    arbitrary = arbExpr
