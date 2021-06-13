module Spaces.SimpleGrammar where

import Spaces.Definition
    ( spNat, Nat, Space((:$:), Pay, (:+:), (:*:)) )
import Util ()

-- Definition of a simple-typed lambda calculus
data Term = App (Term , Term) 
          | Lam Term
          | Var Nat
          deriving (Eq, Show)

-- instance Show Term where
--     show (App (lam, param)) = show lam ++ " " ++ show param
--     show (Lam (var, body))  = "\\" ++ show var ++  ".(" ++ show body ++ ")"
--     show (Var x)            = show x

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
spApp  = App :$: (spLam :*: spTerm) 
spLam  = Lam :$: spTerm
spVar  = Var :$: spNat   
