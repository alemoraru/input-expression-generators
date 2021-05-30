module Spaces.Generator where

import Spaces.Grammar
import Spaces.Definition
import Spaces.Interp

import qualified Test.QuickCheck as QC

-- Predicate that checks if a given term is of a specified type
predicate :: Term -> Bool
predicate = typeCheck (TFun (TFun TInt TInt) TInt) 

-- Get a random lambda term 
getLambdaTerm :: IO Term 
getLambdaTerm = QC.generate $ uniformFilter predicate spTerm 6