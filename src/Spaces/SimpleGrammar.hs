module Spaces.SimpleGrammar where

import Spaces
    ( spNat, Nat, Space((:$:), Pay, Pure, (:+:), (:*:)) )
import Util ()

-- Definition of a simple-typed lambda calculus
data Term = App (Term , Term) 
          | Lam Term
          | Var Int
          deriving (Eq, Show)

-- Representation of a type environment
type TEnvironment = [(Term, Type)]

-- Data typed used for checking type-correctness of lambda terms
data Type = TInt | TFun Type Type
  deriving (Eq)

instance Show Type where
    show TInt = "Int"
    show (TFun param body) = "(" ++ show param ++ " -> " ++ show body ++ ")"

-- The space of all lambda terms
spTerm, spApp, spLam, spVar :: Space Term
spTerm = Pay (spApp :+: spLam :+: spVar)
spApp  = App :$: (spTerm :*: spTerm) 
spLam  = Lam :$: spTerm
spVar  = Var :$: spInt

-- Space of int values (start of from 0)
spInt :: Space Int
spInt = Pay (Pure 0 :+: (succ :$: spInt))
