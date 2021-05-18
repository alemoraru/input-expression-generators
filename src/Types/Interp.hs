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

typeOf (Eq left right) nv = 
    case (typeOf left nv, typeOf right nv) of
        (Right TInt, Right TInt) -> Right TBool
        _ -> Left $ TypeError "Cannot check equality on non-int types."
typeOf (Lt left right) nv = 
    case (typeOf left nv, typeOf right nv) of
        (Right TInt, Right TInt) -> Right TBool
        _ -> Left $ TypeError "Cannot check less-than on non-int types."
typeOf (Gt left right) nv = 
    case (typeOf left nv, typeOf right nv) of
        (Right TInt, Right TInt) -> Right TBool
        _ -> Left $ TypeError "Cannot check greater-than on non-int types."

-- functions

typeOf (Lambda arg body) nv = 
    case typeOf body (replace (fst arg) (snd arg) nv) of
        Right t -> Right $ TClos (snd arg) t
        _ -> Left $ TypeError "Cannot typecheck body of the lambda."
typeOf (App f param) nv = 
    case typeOf f nv of
        Right (TClos arg t) ->
            case typeOf param nv of
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

-- basic building blocks

interp (EInt x) nv  = Right (VInt x)
interp (EBool x) nv = Right (VBool x)
interp (Id str) nv  =
    case lookup str nv of
        (Just val) -> Right val
        _ -> Left $ InterpError $ "Variable " ++ str ++ " does not have a binding"

-- basic operations on ints
interp (Add left right) nv = 
    case (interp left nv, interp right nv) of
        (Right (VInt l), Right (VInt r)) -> Right $ VInt (l + r)
        _ -> Left $ TypeError "Cannot perform addition on non-int types."


interp (Mul left right) nv = 
    case (interp left nv, interp right nv) of
        (Right (VInt l), Right (VInt r)) -> Right $ VInt (l * r)
        _ -> Left $ TypeError "Cannot perform multiplication on non-int types."

-- basic operations on booleans

interp (Not e) nv = 
    case interp e nv of
        Right (VBool b) -> Right $ VBool (not b)
        _ -> Left $ TypeError "Cannot perform the not operation on non-bool type."
interp (Or left right) nv = 
    case (interp left nv, interp right nv) of
        (Right (VBool l), Right (VBool r)) -> Right $ VBool (l || r)
        _ -> Left $ TypeError "Cannot perform the or operation on non-bool types."
interp (And left right) nv = 
    case (interp left nv, interp right nv) of
        (Right (VBool l), Right (VBool r)) -> Right $ VBool (l && r)
        _ -> Left $ TypeError "Cannot perform the and operation on non-bool types."

-- comparisons

interp (Eq left right) nv = 
    case (interp left nv, interp right nv) of
        (Right (VInt l), Right (VInt r)) -> Right $ VBool $ l == r
        _ -> Left $ TypeError "Cannot check equality on non-int types."
interp (Lt left right) nv = 
    case (interp left nv, interp right nv) of
        (Right (VInt l), Right (VInt r)) -> Right $ VBool $ l < r
        _ -> Left $ TypeError "Cannot check less-than on non-int types."
interp (Gt left right) nv = 
    case (interp left nv, interp right nv) of
        (Right (VInt l), Right (VInt r)) -> Right $ VBool $ l > r
        _ -> Left $ TypeError "Cannot check greater-than on non-int types."

-- functions

interp (Lambda arg body) nv = Right $ VClos (fst arg) body nv
interp (App f param) nv =
    case interp f nv of
        Right (VClos arg body _) -> 
            case interp param nv of
                Right p -> interp body nv
                Left _  -> Left $ InterpError "Could not interpret body of lambda."
        _ -> Left $ InterpError "Cannot apply to non-closure type."

-- conditional

interp (If b t f) nv = 
    case interp b nv of
        Right (VBool True)  -> interp t nv
        Right (VBool False) -> interp f nv
        _ -> Left $ TypeError "Cannot interpret non-bool conditional."

safeInterp :: Expr -> Either Error Val
safeInterp expr = 
    do
        _ <- typeOf expr []
        interp expr []
