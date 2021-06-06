module Conditional.Generator where

import Spaces.Definition
    ( uniformFilter, uniform, Space((:+:), (:*:), (:$:), Pay, Pure) )

import Test.QuickCheck ( Arbitrary(arbitrary), Gen )

import Util ( Error (..) )

-- ADT for expressions which also contains conditionals
data Expr =
  -- basic building blocks
  EInt Int | EBool Bool
  -- basic operations on ints
  | Add (Expr, Expr) | Mul (Expr, Expr)
  -- basic operations on booleans
  | Not Expr | Or (Expr, Expr) | And (Expr, Expr)
  -- comparisons
  | Eq (Expr, Expr) | Lt (Expr, Expr) | Gt (Expr, Expr)
  -- conditionals
  | If (Expr, (Expr, Expr))
  deriving ( Eq, Show )

-- ADT for result values
data Val = VInt Int | VBool Bool
  deriving ( Eq, Show )

-- ADT for type values
data Type = TInt | TBool
  deriving ( Eq, Show )

-- Space of conditional expressions
spEInt, spEBool, spAdd, spMul, spNot, spOr, spAnd, spEq, spLt, spGt, spIf, spExpr :: Space Expr
spExpr  = Pay (spEInt :+: spEBool :+: spAdd :+: spMul :+: spOr :+: spNot :+: spEq :+: spLt :+: spGt :+: spIf)
spEInt  = EInt  :$: spInt 
spEBool = EBool :$: spBool 
spAdd   = Add   :$: (spExpr :*: spExpr) 
spMul   = Mul   :$: (spExpr :*: spExpr) 
spNot   = Not   :$: spExpr
spOr    = Or    :$: (spExpr :*: spExpr)
spAnd   = And   :$: (spExpr :*: spExpr)
spEq    = Eq    :$: (spExpr :*: spExpr) 
spLt    = Lt    :$: (spExpr :*: spExpr)
spGt    = Gt    :$: (spExpr :*: spExpr)
spIf    = If    :$: (spExpr :*: (spExpr :*: spExpr)) 

-- Space of int values (start of from 0)
spInt :: Space Int
spInt = Pay (Pure 1 :+: (succ :$: spInt))

-- Space of boolean values (just True & False values)
spBool :: Space Bool
spBool = Pay (Pure True :+: Pure False)

-- Predicate for checking type-correctness
isTypeCorrect :: Expr -> Bool
isTypeCorrect expr = isCorrect 
    where 
        isCorrect = case typeCheck expr of
            Left  _ -> False
            Right _ -> True

-- Auxiliary function for QuickCheck arbitrary function
-- Uses a universally true predicate (no need for type-checking)
arbExpr :: Gen Expr
arbExpr = uniform isTypeCorrect spExpr 5

-- Needed for QuickCheck generation
instance Arbitrary Expr where
    arbitrary = arbExpr

-- Type checker for conditional expressions
typeCheck :: Expr -> Either Error Type
typeCheck (EInt x)  = Right TInt
typeCheck (EBool x) = Right TBool
typeCheck (Add (left, right)) = case (typeCheck left, typeCheck right) of
    (Right TInt, Right TInt) -> Right TInt
    _ -> Left $ TypeError "Type error: addition not performed on ints."
typeCheck (Mul (left, right)) = case (typeCheck left, typeCheck right) of
    (Right TInt, Right TInt) -> Right TInt
    _ -> Left $ TypeError "Type error: multiplication not performed on ints."
typeCheck (Not expr) = case typeCheck expr of
    Right TBool -> Right TBool
    _ -> Left $ TypeError "Type error: not operation performed on non-booleans."
typeCheck (And (left, right)) = case (typeCheck left, typeCheck right) of
    (Right TBool, Right TBool) -> Right TBool
    _ -> Left $ TypeError "Type error: not operation performed on non-booleans."
typeCheck (Or (left, right)) = case (typeCheck left, typeCheck right) of
    (Right TBool, Right TBool) -> Right TBool
    _ -> Left $ TypeError "Type error: not operation performed on non-booleans."
typeCheck (Eq (left, right)) = case (typeCheck left, typeCheck right) of
    (Right TInt, Right TInt) -> Right TBool
    _ -> Left $ TypeError "Type error: eq-comparison performed on non-ints."
typeCheck (Lt (left, right)) = case (typeCheck left, typeCheck right) of
    (Right TInt, Right TInt) -> Right TBool
    _ -> Left $ TypeError "Type error: lt-comparison performed on non-ints."
typeCheck (Gt (left, right)) = case (typeCheck left, typeCheck right) of
    (Right TInt, Right TInt) -> Right TBool
    _ -> Left $ TypeError "Type error: gt-comparison performed on non-ints."
typeCheck (If (i, (t, e))) = case typeCheck i of 
    Right TBool -> case (typeCheck t, typeCheck e) of 
        (Right tt, Right te)   -> if tt == te then Right tt else Left $ TypeError "Type error: types of branches don't match."
        (Left err1, Left err2) -> Left err1
    _ -> Left $ TypeError "Type error: conditional branch was not a boolean."

-- Correct interpretation of conditional expressions
interpC1 :: Expr -> Either Error Val
interpC1 (EInt x)  = Right (VInt x)
interpC1 (EBool x) = Right (VBool x)
interpC1 (Add (left, right)) = case (interpC1 left, interpC1 right) of
    (Right (VInt x), Right (VInt y)) -> Right (VInt $ x + y)
    _ -> Left $ InterpError "Cannot perform addition on non-ints."
interpC1 (Mul (left, right)) = case (interpC1 left, interpC1 right) of
    (Right (VInt x), Right (VInt y)) -> Right (VInt $ x * y)
    _ -> Left $ InterpError "Cannot perform multiplication on non-ints."
interpC1 (Not expr) = case interpC1 expr of
    (Right (VBool x)) -> Right (VBool $ not x)
    _ -> Left $ InterpError "Cannot perform the not operation on non-booleans."
interpC1 (Or (left, right)) = case (interpC1 left, interpC1 right) of
    (Right (VBool x), Right (VBool y)) -> Right (VBool $ x || y)
    _ -> Left $ InterpError "Cannot perform the or operation on non-booleans."
interpC1 (And (left, right)) = case (interpC1 left, interpC1 right) of
    (Right (VBool x), Right (VBool y)) -> Right (VBool $ x && y)
    _ -> Left $ InterpError "Cannot perform the and operation on non-booleans."
interpC1 (Eq (left, right)) = case (interpC1 left, interpC1 right) of
    (Right (VInt x), Right (VInt y)) -> Right (VBool $ x == y)
    _ -> Left $ InterpError "Cannot perform the and operation on non-booleans."
interpC1 (Lt (left, right)) = case (interpC1 left, interpC1 right) of
    (Right (VInt x), Right (VInt y)) -> Right (VBool $ x < y)
    _ -> Left $ InterpError "Cannot perform comparison between non-ints."
interpC1 (Gt (left, right)) = case (interpC1 left, interpC1 right) of
    (Right (VInt x), Right (VInt y)) -> Right (VBool $ x > y)
    _ -> Left $ InterpError "Cannot perform comparison between non-ints."
interpC1 (If (i, (t, e))) = case interpC1 i of
    (Right (VBool cond)) -> if cond then interpC1 t else interpC1 e
    _ -> Left $ InterpError "Cannot evaluate non-boolean condition"