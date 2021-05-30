module Spaces.Grammar where

import Spaces.Definition
import Util

-- Definition of a simple-typed lambda calculus
data Term = App (Term , Term) 
          | Lam (Term, Term) 
          | Var Nat
          deriving (Eq)

instance Show Term where
    show (App (lam, param)) = show lam ++ " " ++ show param
    show (Lam (var, body))  = "\\" ++ show var ++  ".(" ++ show body ++ ")"
    show (Var x)            = show x

-- The space of all lambda terms
spTerm, spApp, spLam, spVar :: Space Term
spTerm = Pay (spApp :+: spLam :+: spVar)
spApp  = App :$: (spTerm :*: spTerm) 
spLam  = Lam :$: (spVar :*: spTerm)
spVar  = Var :$: spNat   
