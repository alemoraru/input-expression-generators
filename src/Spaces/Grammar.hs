module Spaces.Grammar where

import Spaces.Definition
import qualified Test.QuickCheck as QC

-- ADT for STLC
data Expr
    = App ((Expr, Expr), Type)
    | Var Int
    | Lam Expr
    deriving (Eq, Show)

-- ADT for type values
data Type = TInt
          | TFun (Type, Type)
          deriving (Eq, Show)

type TEnvironment = [Type]

-- Type checker
check :: TEnvironment -> Type -> Expr -> Bool
check env t (Var i)               = env !! i == t
check env t (App ((f, x), tx))    =
    check env (TFun (tx, t)) f && check env tx x
check env (TFun (ta, tb)) (Lam e) = check (ta : env) tb e
check _   _                _      = False

-- Sample type environment to use when generating data
sampleEnv :: TEnvironment
sampleEnv =
    [
        TInt,
        TInt,
        TFun (TInt, TInt),
        TFun (TInt, TInt),
        TFun (TFun (TInt,TInt), TInt)
    ]

-- The space of all lambda terms
spTerm, spApp, spLam, spVar :: Space Expr
spTerm = Pay (spApp :+: spLam :+: spVar)
spApp  = App :$: (spLam :*: spTerm :*: spType)
spLam  = Lam :$: spTerm
spVar  = Var :$: spInt

-- The space for type values
spType, spTInt, spTFun :: Space Type
spType = Pay (spTInt :+: spTFun)
spTInt = Pure TInt
spTFun = TFun :$: (spType :*: spType)

-- The space for ints
spInt :: Space Int
spInt = Pay (Pure 0 :+: (succ :$: spInt))