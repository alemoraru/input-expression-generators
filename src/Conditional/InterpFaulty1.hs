module Conditional.InterpFaulty1 where

import Conditional.Grammar ( Environment, Expr(..), Val(..) )
import Util ( Error(InterpError) )

-- Search for a binding within an environment
findId :: String -> Environment -> Either Error Val
findId str [] = Left $ InterpError $ "Variable " ++ str ++ " not found."
findId str ((name, val) : envRest) = if name == str then Right val else findId name envRest

-- Faulty interpretation of an expression 
interp :: Expr -> Environment -> Either Error Val
interp (EInt x) nv  = Right (VInt x)
interp (EBool b) nv = Right (VBool b)
interp (Id str) nv  = findId str nv

interp (Add (e1, e2)) nv =
  case (interp e1 nv, interp e2 nv) of
    (Right (VInt v1), Right (VInt v2)) -> Right (VInt (v1 + v2))
    _ -> Left $ InterpError "Cannot perform addition on non-ints."
interp (Mul (e1, e2)) nv =
  case (interp e1 nv, interp e2 nv) of
    (Right (VInt v1), Right (VInt v2)) -> Right (VInt (v1 * v2))
    _ -> Left $ InterpError "Cannot perform multiplication on non-ints."

interp (Not e) nv =
  case interp e nv of
    (Right (VBool v)) -> Right (VBool (not v))
    _ -> Left $ InterpError "Cannot perform not operation on non-booleans."
interp (Or (e1, e2)) nv =
  case (interp e1 nv, interp e2 nv) of
    (Right (VBool v1), Right (VBool v2)) -> Right (VBool (v1 || v2))
    _ -> Left $ InterpError "Cannot perform or operation on non-booleans."
interp (And (e1, e2)) nv =
  case (interp e1 nv, interp e2 nv) of
    (Right (VBool v1), Right (VBool v2)) -> Right (VBool (v1 && v2))
    _ -> Left $ InterpError "Cannot perform and operation on non-booleans."

interp (Eq (e1, e2)) nv =
  case (interp e1 nv, interp e2 nv) of
    (Right (VInt v1), Right (VInt v2)) -> Right (VBool (v1 == v2))
    _ -> Left $ InterpError "Cannot perform equality check on non-ints."
interp (Lt (e1, e2)) nv =
  case (interp e1 nv, interp e2 nv) of
    (Right (VInt v1), Right (VInt v2)) -> Right (VBool (v1 < v2))
    _ -> Left $ InterpError "Cannot perform number comparison on non-ints."
interp (Gt (e1, e2)) nv =
  case (interp e1 nv, interp e2 nv) of
    (Right (VInt v1), Right (VInt v2)) -> Right (VBool (v1 > v2))
    _ -> Left $ InterpError "Cannot perform number comparison on non-ints."

interp (Lambda (str, e)) nv = Right (VClos (fst str) e nv)
interp (App (e1, e2)) nv =
  case interp e1 nv of
    Right (VClos str e newEnv) ->
      case interp e2 nv of
        (Right interpVal) -> interp e ((str, interpVal) : newEnv)
        _ -> Left $ InterpError "Cannot interpret body of lambda."
    _ -> Left $ InterpError "Cannot apply to non-closure type."

interp (If (e1, (e2, e3))) nv =
  case interp e1 nv of
    Right (VBool bool) -> if bool then interp e3 nv else interp e2 nv -- introduced error (swapped branches)
    _ -> Left $ InterpError "Cannot interpret non-boolean condition."
