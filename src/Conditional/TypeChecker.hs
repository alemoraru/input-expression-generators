module Conditional.TypeChecker where

import Conditional.Grammar

import Util

typeCheck :: Expr -> TEnvironment -> Either Error Type
typeCheck (EInt _) nv  = Right TInt
typeCheck (EBool _) nv = Right TBool
typeCheck (Id str) nv = 
    case lookup str nv of 
        Just t -> Right t
        _      -> Left $ TypeError $ "Variable " ++ str ++ " does not have a type binding."
typeCheck (Add (left, right)) nv = 
    case (typeCheck left nv, typeCheck right nv) of
        (Right TInt , Right TInt) -> Right TInt 
        _ -> Left $ TypeError "Cannot perform addition on non-int types."
typeCheck (Mul (left, right)) nv = 
    case (typeCheck left nv, typeCheck right nv) of
        (Right TInt , Right TInt) -> Right TInt 
        _ -> Left $ TypeError "Cannot perform multiplication on non-int types."

-- basic operations on booleans

typeCheck (Not b) nv = 
    case typeCheck b nv of 
        Right TBool -> Right TBool 
        _ -> Left $ TypeError "Cannot perform the not operation on non-bool type."
typeCheck (And (left, right)) nv = 
    case (typeCheck left nv, typeCheck right nv) of
        (Right TBool, Right TBool) -> Right TBool
        _ -> Left $ TypeError "Cannot perform the and operation on non-bool types."
typeCheck (Or (left, right)) nv = 
    case (typeCheck left nv, typeCheck right nv) of
        (Right TBool, Right TBool) -> Right TBool
        _ -> Left $ TypeError "Cannot perform the or operation on non-bool types."

-- comparisons

typeCheck (Eq (left, right)) nv = 
    case (typeCheck left nv, typeCheck right nv) of
        (Right TInt, Right TInt) -> Right TBool
        _ -> Left $ TypeError "Cannot check equality on non-int types."
typeCheck (Lt (left, right)) nv = 
    case (typeCheck left nv, typeCheck right nv) of
        (Right TInt, Right TInt) -> Right TBool
        _ -> Left $ TypeError "Cannot check less-than on non-int types."
typeCheck (Gt (left, right)) nv = 
    case (typeCheck left nv, typeCheck right nv) of
        (Right TInt, Right TInt) -> Right TBool
        _ -> Left $ TypeError "Cannot check greater-than on non-int types."

-- functions

typeCheck (Lambda arg body) nv = 
    case typeCheck body (replace (fst arg) (snd arg) nv) of
        Right t -> Right $ TClos (snd arg) t
        _ -> Left $ TypeError "Cannot typecheck body of the lambda."
typeCheck (App (f, param)) nv = 
    case typeCheck f nv of
        Right (TClos arg t) ->
            case typeCheck param nv of
                Right paramType -> 
                    if arg == paramType 
                        then Right t
                        else Left $ TypeError "The types of the conditonal branches don't match."
                _ -> Left $ TypeError "Input parameters to application don't typecheck."
        _ -> Left $ TypeError "Cannot apply to non-closure type."

-- conditional
typeCheck (If (b, (t, f))) nv = 
    case (typeCheck b nv, typeCheck t nv, typeCheck f nv) of
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
