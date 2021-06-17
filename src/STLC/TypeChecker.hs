module STLC.TypeChecker where

import STLC.Grammar ( Type(TFun), Expr(..), TEnvironment )

-- Type checker
typeCheck :: TEnvironment -> Type -> Expr -> Bool
typeCheck env t (Var i)               = env !! i == t
typeCheck env t (App ((f, x), tx))    =
    typeCheck env (TFun (tx, t)) f && typeCheck env tx x
typeCheck env (TFun (ta, tb)) (Lam e) = typeCheck (ta : env) tb e
typeCheck _   _                _      = False