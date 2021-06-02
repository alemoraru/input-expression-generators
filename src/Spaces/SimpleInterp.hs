module Spaces.SimpleInterp where

import Spaces.SimpleGrammar

-- Check that a Term is of a given type
typeCheck :: Type -> Term -> Bool
typeCheck TInt expr = case expr of
    Var _            -> True 
    App (lam, param) -> False
    _                -> False 
typeCheck (TFun argTy retTy) expr = case expr of 
    Lam body -> case body of
        Var _ -> True 
        _     -> False   
    App (lam, param) -> False  
    _                -> False  

-- Checks if a given Term is properly typed
-- typeOf :: Term -> Type
-- typeOf (Var _)    = TInt 
-- typeOf (Lam body) = TFun TInt (typeOf    )
-- typeOf (App (lam, param)) = 