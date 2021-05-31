module Spaces.SimpleInterp where

import Spaces.SimpleGrammar

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