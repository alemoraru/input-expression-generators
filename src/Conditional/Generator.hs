module Conditional.Generator where

import Conditional.Grammar
    ( Environment,
      Expr(Id, EInt, EBool, Add, Mul, Not, Or, And, Eq, Lt, Gt, If, Lambda, App),
      Val(VBool, VInt), TEnvironment, Type (TInt, TBool) )
import Conditional.TypeChecker ( typeCheck )
import Spaces ( uniformFilter, uniform, Space((:+:), (:*:), (:$:), Pay, Pure) )
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
    arbitrary = frequency [(0, sized arbNaiveExpr), (1, arbUniformExpr)]

-- Auxiliary function for QuickCheck arbitrary function
-- Uses a universally true predicate (no need for type-checking)
arbUniformExpr :: Gen Expr
arbUniformExpr = uniform isTypeCorrect spExpr 1

-- Function for generating data 
-- of a particular depth 
arbNaiveExpr :: Int -> Gen Expr
arbNaiveExpr 0 = oneof [EInt <$> arbitrary, EBool <$> arbitrary, getArbVar sampleEnv]
arbNaiveExpr n = frequency
  [
    (1, oneof [EInt <$> arbitrary, EBool <$> arbitrary, getArbVar sampleEnv]) -- Leaf generation (lowest prob to generate)
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
  , (4, do
          str  <- getArbId sampleEnv
          body <- arbNaiveExpr (n `div` 2)
          ty   <- case lookup str sampleTEnv of
                  Just t  -> return t
                  Nothing -> getArbTy sampleTEnv
          return $ Lambda ((str, ty), body))
   , (4, do
          lam <- arbNaiveExpr (n `div` 2)
          arg <- arbNaiveExpr (n `div` 2)
          return $ App (lam, arg))
  ]

-- Get an arbitrary variable from the available environment
getArbVar :: Environment -> Gen Expr
getArbVar nv = 
  do 
    (key, val) <- elements nv
    return (Id key) 

-- Get an arbitrary identifier from the available environment
getArbId :: Environment -> Gen String 
getArbId nv = 
  do
    (key, val) <- elements nv
    return key

-- Get an arbitrary type from the available environment
getArbTy :: TEnvironment -> Gen Type 
getArbTy nv =
  do
    (key, ty) <- elements nv
    return ty

-- Environment used for testing interpretation
sampleEnv :: Environment 
sampleEnv = 
  [
    ("zero", VInt 0),
    ("one", VInt 1),
    ("two", VInt 2),
    ("three", VInt 3),
    ("tru", VBool True),
    ("fls", VBool False)
  ]

-- Type environment used for testing
sampleTEnv :: TEnvironment 
sampleTEnv =
  [
    ("zero", TInt),
    ("one", TInt),
    ("two", TInt),
    ("three", TInt),
    ("tru", TBool),
    ("fls", TBool)
  ]