module Spaces.SimpleGenerator where

import Spaces.SimpleGrammar
import Spaces.Definition
import Spaces.SimpleInterp

import qualified Test.QuickCheck as QC

-- Predicate that checks if a given term is of a specified type
predicate :: Term -> Bool
predicate = typeCheck (TFun TInt TInt)

-- Get a random lambda term 
getTerm :: IO Term  
getTerm = QC.generate $ uniformFilter predicate spTerm 7

getTermFaster :: IO Term  
getTermFaster = QC.generate $ uniform predicate spTerm 11

-- Auxiliary function for QuickCheck arbitrary function
arbExpr :: QC.Gen Term  
arbExpr = uniformFilter predicate spTerm 7

-- Useful for QuickCheck properties
instance QC.Arbitrary Term  where
    arbitrary = arbExpr
