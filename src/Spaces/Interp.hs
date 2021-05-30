module Spaces.Interp where

import Spaces.Grammar

-- Representation of a type environment
type TEnvironment = [(Term, Type)]

-- Data typed used for checking type-correctness of lambda terms
data Type = TInt | TFun Type Type
  deriving (Eq)

instance Show Type where
    show TInt = "Int"
    show (TFun param body) = "(" ++ show param ++ " -> " ++ show body ++ ")"

-- Check that a Term is of a given type
typeCheck :: Type -> Term -> Bool
typeCheck TInt expr = case expr of
    Var _ -> True 
    _     -> False 
typeCheck (TFun argTy retTy) expr = case expr of 
    Lam (var, body) -> case body of
        Var _ -> True 
        _     -> False   
    App (lam, param) -> False  
    _                -> False  

-- Checks if a given Term is properly typed
typeOf :: Term -> TEnvironment -> Type
typeOf _ _ = undefined 