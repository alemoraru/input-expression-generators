module Conditional.Interp1 where

import Conditional.Grammar
import Lib

findId :: String -> Environment -> Either Error Val
findId str [] = Left $ MissingIdError $ "Variable " ++ str ++ " not found."
findId str ((name, val) : envRest) = if name == str then Right val else findId name envRest

interp :: Expr -> Environment -> Either Error Val

-- basic building blocks
interp (EInt x) nv  = Right (VInt x)
interp (EBool b) nv = Right (VBool b)
interp (Id str) nv  = findId str nv

-- basic operations on ints
interp (Add e1 e2) nv =
  case (interp e1 nv, interp e2 nv) of
    (Right (VInt v1), Right (VInt v2)) -> Right (VInt (v1 + v2))
    _ -> Left $ TypeError "Cannot perform addition on non-ints."
interp (Mul e1 e2) nv =
  case (interp e1 nv, interp e2 nv) of
    (Right (VInt v1), Right (VInt v2)) -> Right (VInt (v1 * v2))
    _ -> Left $ TypeError "Cannot perform multiplication on non-ints."

-- basic operations on boolean
interp (Not e) nv =
  case interp e nv of
    (Right (VBool v)) -> Right (VBool (not v))
    _ -> Left $ TypeError "Cannot perform not operation on non-booleans."
interp (Or e1 e2) nv =
  case (interp e1 nv, interp e2 nv) of
    (Right (VBool v1), Right (VBool v2)) -> Right (VBool (v1 || v2))
    _ -> Left $ TypeError "Cannot perform or operation on non-booleans."
interp (And e1 e2) nv =
  case (interp e1 nv, interp e2 nv) of
    (Right (VBool v1), Right (VBool v2)) -> Right (VBool (v1 && v2))
    _ -> Left $ TypeError "Cannot perform and operation on non-booleans."

-- comparisons
interp (Eq e1 e2) nv =
  case (interp e1 nv, interp e2 nv) of
    (Right (VInt v1), Right (VInt v2)) -> Right (VBool (v1 == v2))
    _ -> Left $ TypeError "Cannot perform equality check on non-ints."

interp (Lt e1 e2) nv =
  case (interp e1 nv, interp e2 nv) of
    (Right (VInt v1), Right (VInt v2)) -> Right (VBool (v1 < v2))
    _ -> Left $ TypeError "Cannot perform number comparison on non-ints."

interp (Gt e1 e2) nv =
  case (interp e1 nv, interp e2 nv) of
    (Right (VInt v1), Right (VInt v2)) -> Right (VBool (v1 > v2))
    _ -> Left $ TypeError "Cannot perform number comparison on non-ints."

-- functions
interp (Lambda str e) nv = Right (VClos str e nv)

interp (App e1 e2) nv =
  case interp e1 nv of
    Right (VClos str e newEnv) ->
      case interp e2 nv of
        (Right interpVal) -> interp e ((str, interpVal) : newEnv)
        _ -> Left $ TypeError "Cannot interpret body of lambda."
    _ -> Left $ TypeError "Cannot apply to non-closure type."

-- conditionals
interp (If e1 e2 e3) nv =
  case interp e1 nv of
    Right (VBool bool) ->
      case bool of
        True  -> interp e2 nv
        False -> interp e3 nv
    _ -> Left $ TypeError "Cannot interpret non-boolean condition."
