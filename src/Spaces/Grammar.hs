module Spaces.Grammar where

import Spaces.Definition

-- Definition of a simple-typed lambda calculus
data Term = App (Term , Term) 
          | Lam Term 
          | Var Nat
          deriving (Eq)

instance Show Term where
    show (App (lam, param)) = show lam ++ " " ++ show param
    show (Lam body) = "\\(" ++ show body ++ ")"
    show (Var x)    = show x

-- The space of all lambda terms
spTerm, spApp, spLam, spVar :: Space Term
spTerm = Pay (spApp :+: spLam :+: spVar)
spApp  = App :$: (spTerm :*: spTerm) 
spLam  = Lam :$: spTerm
spVar  = Var :$: spNat   


-- Data typed used for checking type-correctness of lambda terms
data Type = TInt | TFun Type Type
  deriving (Eq)

instance Show Type where
    show TInt = "Int"
    show (TFun param body) = "(" ++ show param ++ " -> " ++ show body ++ ")"

-- Check that a Term is of a given type
typeOf :: Type -> Term -> Bool
typeOf TInt expr = case expr of
    Var _ -> True 
    _     -> False 
typeOf (TFun argTy retTy) expr = case expr of 
    Lam body         -> case body of
        Var _ -> True 
        _     -> False   
    App (lam, param) -> False  
    _                -> False  