module UntypedLambda.Interp where

import UntypedLambda.Grammar
import Util

interp :: Expr -> Environment -> Either Error Val 
interp (Id x) env = 
    case lookup x env of
        (Just val) -> Right val
        _          -> Left $ InterpError $ "Variable " ++ x ++ " not found."

interp (Lambda arg body) env = Right $ VClos arg body env

interp (App fun param) env   = 
    case interp fun env of 
        Right (VClos arg body newEnv) -> 
            case interp param env of
                Right interpParam -> interp body ((arg, interpParam) : newEnv)
                _                 -> Left $ InterpError "Cannot interpret body of lambda."
        _  -> Left $ InterpError "Cannot apply to non-closure type."
        