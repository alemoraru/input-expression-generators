module Spaces.Grammar where

import Spaces ( Space((:+:), (:*:), Pay, Pure, (:$:)) )

-- Alias for Type Environment
type TEnvironment = [Type]
type Environment  = [Val]

-- ADT for SLC result values
data Val = VInt Int | VClos Int Expr Environment
    deriving (Eq, Show)

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
spInt = Pay $ pureInts (length sampleTEnv) 

pureInts :: Int -> Space Int
pureInts 0  = Pure 0
pureInts n  = Pure (n - 1) :+: pureInts (n - 1)


-- Sample type environment to use when generating data
sampleTEnv :: TEnvironment
sampleTEnv =
    [
        TInt,
        TInt,
        TFun (TInt, TInt),
        TFun (TInt, TInt),
        TInt,
        TFun (TInt, TFun (TInt,TInt))
    ]
