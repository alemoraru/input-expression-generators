module Conditional.InterpFaulty2 where

import Conditional.Grammar ( Environment, Expr(..), Val(..) )
import Util ( Error(InterpError) )

-- Faulty interpretation of an expression 
interp :: Expr -> Environment -> Either Error Val
interp (EInt x) nv  = Right (VInt x)
interp (EBool b) nv = Right (VBool b)

interp (Add (e1, e2)) nv = case (interp e1 nv, interp e2 nv) of
                          (Right (VInt v1), Right (VInt v2)) -> Right (VInt (v1 + v2))
                          _ -> Left $ InterpError "Cannot perform addition on non-ints."
interp (Mul (e1, e2)) nv = case (interp e1 nv, interp e2 nv) of
                          (Right (VInt v1), Right (VInt v2)) -> Right (VInt (v1 * v2))
                          _ -> Left $ InterpError "Cannot perform multiplication on non-ints."

interp (Not e) nv = case interp e nv of
                          (Right (VBool v)) -> Right (VBool (not v))
                          _ -> Left $ InterpError "Cannot perform not operation on non-booleans."
interp (Or (e1, e2)) nv = case (interp e1 nv, interp e2 nv) of
                           (Right (VBool v1), Right (VBool v2)) -> Right (VBool (v1 || v2))
                           _ -> Left $ InterpError "Cannot perform or operation on non-booleans."
interp (And (e1, e2)) nv = case (interp e1 nv, interp e2 nv)  of
                            (Right (VBool v1), Right (VBool v2)) -> Right (VBool (v1 && v2))
                            _ -> Left $ InterpError "Cannot perform and operation on non-booleans."

interp (If (e1, (e2, e3))) nv = case interp e1 nv of
                            Right (VBool True)  -> interp e2 nv
                            Right (VBool False) -> interp e2 nv -- error here (same branch for else branch)
                            _ -> Left $ InterpError "Cannot interpret non-boolean condition."

interp (Eq (e1, e2)) nv = case (interp e1 nv, interp e2 nv)  of
                            (Right (VInt v1), Right (VInt v2)) -> Right (VBool (v1 == v2))
                            _ -> Left $ InterpError "Cannot perform equality check on non-ints."

interp (Lt (e1, e2)) nv = case (interp e1 nv, interp e2 nv)  of
                            (Right (VInt v1), Right (VInt v2)) -> Right (VBool (v1 < v2))
                            _ -> Left $ InterpError "Cannot perform number comparison on non-ints."

interp (Gt (e1, e2)) nv = case (interp e1 nv, interp e2 nv) of
                            (Right (VInt v1), Right (VInt v2)) -> Right (VBool (v1 > v2))
                            _ -> Left $ InterpError "Cannot perform number comparison on non-ints."

interp (Lambda (str, e)) nv = Right (VClos (fst str) e nv)

interp (App (e1, e2)) nv = case interp e1 nv of
                            Right (VClos str e newEnv) -> case interp e2 nv of
                              (Right interpVal) -> interp e (nv ++ [(str, interpVal)])
                              _ -> Left $ InterpError "Cannot interpret body of lambda."
                            _ -> Left $ InterpError "Cannot apply to non-closure type."

interp (Id s) nv = case lookup s nv of
                       (Just val) -> Right val
                       _ -> Left $ InterpError $ "Variable " ++ s ++ " not found."