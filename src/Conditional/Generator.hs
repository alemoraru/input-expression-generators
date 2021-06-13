module Conditional.Generator where

import Conditional.Grammar
    ( Environment,
      Expr(Id, EInt, EBool, Add, Mul, Not, Or, And, Eq, Lt, Gt, If),
      Val(VBool, VInt) )
import Conditional.TypeChecker ( typeCheck )

import Spaces.Definition
    ( uniformFilter, uniform, Space((:+:), (:*:), (:$:), Pay, Pure) )

import Test.QuickCheck ( Arbitrary(arbitrary), Gen, frequency, elements, oneof, sized )

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
        isCorrect = case typeCheck expr [] of
            Left  _ -> False
            Right _ -> True

-- Needed for QuickCheck generation
-- Weight 1 for naive generation and 4 for uniform generation
instance Arbitrary Expr where
    arbitrary = frequency [(1, sized arbNaiveExpr), (4, arbUniformExpr)]

-- Auxiliary function for QuickCheck arbitrary function
-- Uses a universally true predicate (no need for type-checking)
arbUniformExpr :: Gen Expr
arbUniformExpr = uniform isTypeCorrect spExpr 5

-- Function for generating data 
-- of a particular depth 
arbNaiveExpr :: Int -> Gen Expr
arbNaiveExpr 0 = oneof [EInt <$> arbitrary, EBool <$> arbitrary]
arbNaiveExpr n = frequency
  [
    (1, oneof [EInt <$> arbitrary, EBool <$> arbitrary, arbVar sampleEnvironment]) -- Leaf generation (lowest prob to generate)
  , (4, do 
          left  <- arbNaiveExpr (n `div` 2)
          right <- arbNaiveExpr (n `div` 2)
          return $ Add (left, right))
  , (4, do 
          left  <- arbNaiveExpr (n `div` 2)
          right <- arbNaiveExpr (n `div` 2)
          return $ Mul (left, right))
  , (4, Not <$> arbNaiveExpr (n `div` 2))
  , (4, do 
          left  <- arbNaiveExpr (n `div` 2)
          right <- arbNaiveExpr (n `div` 2)
          return $ Or (left, right))
  , (4, do 
          left  <- arbNaiveExpr (n `div` 2)
          right <- arbNaiveExpr (n `div` 2)
          return $ And (left, right))
  , (4, do 
          left  <- arbNaiveExpr (n `div` 2)
          right <- arbNaiveExpr (n `div` 2)
          return $ Eq (left, right))
  , (4, do 
          left  <- arbNaiveExpr (n `div` 2)
          right <- arbNaiveExpr (n `div` 2)
          return $ Lt (left, right))
  , (4, do 
          left  <- arbNaiveExpr (n `div` 2)
          right <- arbNaiveExpr (n `div` 2)
          return $ Gt (left, right))
  , (4, do 
          i <- arbNaiveExpr (n `div` 2)
          t <- arbNaiveExpr (n `div` 2)
          e <- arbNaiveExpr (n `div` 2)
          return $ If (i, (t, e)))
  ]

-- Get an arbitrary variable from the available environment
arbVar :: Environment -> Gen Expr
arbVar nv = 
  do 
    (key, val) <- elements nv
    return (Id key) 

-- Environment used for testing interpretation
sampleEnvironment :: Environment 
sampleEnvironment = 
  [
    ("zero", VInt 0),
    ("one", VInt 1),
    ("two", VInt 2),
    ("three", VInt 3),
    ("tru", VBool True),
    ("fls", VBool False)
  ]