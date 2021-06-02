{-# LANGUAGE EmptyCase #-}

module Spaces.Interp where

import Spaces.Grammar

-- Alias for Type Environment
type TEnvironment = [Type]

data Val = VInt Int | VClos Int Expr Environment
    deriving (Eq, Show)

type Environment = [Val]

-- Type checker
typeCheck :: TEnvironment -> Type -> Expr -> Bool
typeCheck env t (Var i)               = env !! i == t
typeCheck env t (App ((f, x), tx))    =
    typeCheck env (TFun (tx, t)) f && typeCheck env tx x
typeCheck env (TFun (ta, tb)) (Lam e) = typeCheck (ta : env) tb e
typeCheck _   _                _      = False

interp :: Environment -> Expr -> Val
interp env (Var i)            = env !! i 
interp env (App ((f, x), tx)) =
    case interp env f of 
        VClos i expr newEnv -> 
            case interp env x of
                interpVal -> interp (newEnv ++ [interpVal]) expr
interp env (Lam e)            = VClos (length env) e env 


-- Sample type environment to use when generating data
sampleTEnv :: TEnvironment
sampleTEnv =
    [
        TInt,
        TInt,
        TFun (TInt, TInt),
        TFun (TInt, TInt),
        TFun (TInt, TFun (TInt,TInt))
    ]
-- Sample environment for intepretation
sampleEnv :: Environment
sampleEnv =
    [
        VInt 1,
        VInt 42
    ]
