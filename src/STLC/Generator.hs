module STLC.Generator where

import Spaces.Definition ( Space((:+:), (:*:), Pay, Pure, (:$:)) )
import STLC.Grammar ( Type(..), Expr(..) )

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