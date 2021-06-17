module Spaces.Grammar where

import Spaces ( Space((:+:), (:*:), Pay, Pure, (:$:)) )

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
spInt = Pay (Pure 0 :+: (succ :$: spInt))