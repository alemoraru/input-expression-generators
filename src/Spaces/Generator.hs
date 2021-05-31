module Spaces.Generator where

import Spaces.SimpleGrammar
import Spaces.Definition
import Spaces.SimpleInterp

import qualified Test.QuickCheck as QC

-- Predicate that checks if a given term is of a specified type
predicate :: Term -> Bool
predicate = typeCheck (TFun TInt TInt) 

-- Get a random lambda term 
getLambdaTerm :: IO Term 
getLambdaTerm = QC.generate $ uniformFilter predicate spTerm 8
