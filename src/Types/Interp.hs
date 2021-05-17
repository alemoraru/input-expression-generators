module Types.Interp where

import Types.Grammar
import Util

typeOf :: Expr -> TEnvironment -> Either Error Type
typeOf (EInt _) nv  = Right TInt
typeOf (EBool _) nv = Right TBool
typeOf (Id str) nv = 
    case lookup str nv of 
        Just t -> Right t
        _      -> Left $ TypeError $ "Variable " ++ str ++ " does not have a type binding."

typeOf (Add left right) nv = 
    case (typeOf left nv, typeOf right nv) of
        (Right TInt , Right TInt) -> Right TInt 
        _ -> Left $ TypeError "Cannot perform addition on non-int types."
typeOf (Mul left right) nv = 
    case (typeOf left nv, typeOf right nv) of
        (Right TInt , Right TInt) -> Right TInt 
        _ -> Left $ TypeError "Cannot perform multiplication on non-int types."

typeOf (Not b) nv = 
    case typeOf b nv of 
        Right TBool -> Right TBool 
        _ -> Left $ TypeError "Cannot perform the not operation on non-bool type."

typeOf _ _ = undefined 

interp :: Expr -> Environment -> Either Error Val
interp = undefined

safeInterp :: Expr -> Either Error Val
safeInterp = undefined