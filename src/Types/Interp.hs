module Types.Interp where

import Types.Grammar
import Util

typeOf :: Expr -> TEnvironment -> Either Error Type

-- basic building blocks

typeOf (EInt _) nv  = Right TInt
typeOf (EBool _) nv = Right TBool
typeOf (Id str) nv = 
    case lookup str nv of 
        Just t -> Right t
        _      -> Left $ TypeError $ "Variable " ++ str ++ " does not have a type binding."

-- basic operations on ints

typeOf (Add left right) nv = 
    case (typeOf left nv, typeOf right nv) of
        (Right TInt , Right TInt) -> Right TInt 
        _ -> Left $ TypeError "Cannot perform addition on non-int types."
typeOf (Mul left right) nv = 
    case (typeOf left nv, typeOf right nv) of
        (Right TInt , Right TInt) -> Right TInt 
        _ -> Left $ TypeError "Cannot perform multiplication on non-int types."

-- basic operations on booleans

typeOf (Not b) nv = 
    case typeOf b nv of 
        Right TBool -> Right TBool 
        _ -> Left $ TypeError "Cannot perform the not operation on non-bool type."
typeOf (And left right) nv = 
    case (typeOf left nv, typeOf right nv) of
        (Right TBool, Right TBool) -> Right TBool
        _ -> Left $ TypeError "Cannot perform the and operation on non-bool types."
typeOf (Or left right) nv = 
    case (typeOf left nv, typeOf right nv) of
        (Right TBool, Right TBool) -> Right TBool
        _ -> Left $ TypeError "Cannot perform the or operation on non-bool types."

-- comparisons

typeOf (Eq left right) env = 
    case (typeOf left env, typeOf right env) of
        (Right TInt, Right TInt) -> Right TBool
        _ -> Left $ TypeError "Cannot check equality on non-int types."
typeOf (Lt left right) env = 
    case (typeOf left env, typeOf right env) of
        (Right TInt, Right TInt) -> Right TBool
        _ -> Left $ TypeError "Cannot check less-than on non-int types."
typeOf (Gt left right) env = 
    case (typeOf left env, typeOf right env) of
        (Right TInt, Right TInt) -> Right TBool
        _ -> Left $ TypeError "Cannot check greater-than on non-int types."

-- functions

typeOf (Lambda arg body) env = 
    case typeOf body (replace (fst arg) (snd arg) env) of
        Right t -> Right $ TClos (snd arg) t
        _ -> Left $ TypeError "Cannot typecheck body of the lambda."
typeOf (App f param) env = 
    case typeOf f env of
        Right (TClos arg t) ->
            case typeOf param env of
                Right paramType -> 
                    if arg == paramType 
                        then Right t
                        else Left $ TypeError "The types of the conditonal branches don't match."
                _ -> Left $ TypeError "Input parameters to application don't typecheck."
        _ -> Left $ TypeError "Cannot apply to non-closure type."

-- conditional
typeOf (If b t f) nv = 
    case (typeOf b nv, typeOf t nv, typeOf f nv) of
        (Right TBool, Right left, Right right) -> 
            if left == right 
                then Right left
                else Left $ TypeError "The types of the conditonal branches don't match."
        _ -> Left $ TypeError "Conditional types don't typecheck."

replace :: String -> a -> [(String, a)] -> [(String, a)]
replace key value [] = [(key, value)]
replace key value ((k, v):xs)
  | k == key = (key, value) : xs
  | otherwise = (k, v) : replace key value xs


interp :: Expr -> Environment -> Either Error Val
interp = undefined

safeInterp :: Expr -> Either Error Val
safeInterp = undefined