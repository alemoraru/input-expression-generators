module STLC.Generator where

import Spaces ( Space((:+:), (:*:), Pay, Pure, (:$:)), uniform )
import STLC.Grammar ( Type(..), Expr(..), TEnvironment )
import STLC.TypeChecker ( typeCheck )
import Test.QuickCheck
    ( sized, frequency, Arbitrary(arbitrary), Gen )

-- The space of all lambda terms
spExpr, spApp, spLam, spVar :: Space Expr
spExpr = Pay (spApp :+: spLam :+: spVar)
spApp  = App :$: (spExpr :*: spExpr)
spLam  = Lam :$: ((spInt :*: spType) :*: spExpr)
spVar  = Var :$: spInt

-- The space for type values
spType, spTInt, spTBool, spTFun :: Space Type
spType  = Pay (spTInt :+: spTBool :+: spTFun)
spTInt  = Pure TInt
spTBool = Pure TBool
spTFun  = TFun :$: (spType :*: spType)

-- The space for ints
spInt :: Space Int
spInt = Pay (Pure 0 :+: (succ :$: spInt))

-- Predicate for checking type-correctness
isTypeCorrect :: Expr -> Bool
isTypeCorrect expr = case typeCheck expr sampleTEnv of
    Left  _ -> False
    Right _ -> True

-- Needed for QuickCheck generation
instance Arbitrary Expr where
    arbitrary = arbUniformExpr

-- Auxiliary function for QuickCheck arbitrary function
arbUniformExpr :: Gen Expr
arbUniformExpr = uniform isTypeCorrect spExpr 7

-- Sample Type Environment
sampleTEnv :: TEnvironment
sampleTEnv =
    [
        TInt,                            -- 1
        TBool,                           -- True
        TFun (TInt, TBool),              -- isZero
        TFun (TBool, TInt),              -- boolToInt
        TFun (TInt, TFun (TInt, TInt)),  -- addition
        TFun (TInt, TFun (TInt, TInt))   -- multiplication
    ]