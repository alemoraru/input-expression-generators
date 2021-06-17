module STLC.Grammar where

type TEnvironment = [Type]
type Environment  = [Val]

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

-- ADT for STLC result values
data Val = VInt Int | VClos Int Expr Environment
    deriving (Eq, Show)