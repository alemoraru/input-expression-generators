module STLC.TypeChecker where

import STLC.Grammar ( Type(TFun), Expr(..), TEnvironment, Environment )
import Util ( Error(TypeError) ) 

-- Type checker
typeCheck :: Expr -> TEnvironment -> Either Error Type
typeCheck (Var i) env = 
    if length env <= i 
        then Left $ TypeError $ "No binding found for variable " ++ show i ++ "." 
        else Right $ env !! i
typeCheck (Lam ((v, t), body)) env = 
    do
        tBody <- typeCheck body env
        Right $ TFun (t, tBody)
typeCheck (App (lam, arg)) env = 
    do
        tLam <- typeCheck lam env
        case tLam of
            TFun (tParam, tBody) ->
                do
                    tArg <- typeCheck arg env
                    if tArg == tParam
                        then Right tBody
                        else Left $ TypeError "Application does not match parameter type."
            _ -> Left $ TypeError "Cannot apply parameter to non-lambda type."
