module Spaces.Generator where

import Spaces.Grammar
import Spaces.Definition
import Spaces.Interp

import qualified Test.QuickCheck as QC

-- Predicate that checks if a given term is of a specified type
predicate :: Expr -> Bool
predicate = check sampleEnv (TFun (TInt, TInt)) 

-- Get a random lambda term 
getLambdaTerm :: IO Expr 
getLambdaTerm = QC.generate $ uniformFilter predicate spTerm 7
