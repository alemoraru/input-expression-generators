module STLC.Grammar where

type TEnvironment = [Type]
type Environment  = [Val]

-- ADT for STLC
data Expr = Var Int
  | Lam ((Int, Type), Expr)
  | App (Expr, Expr)
  deriving ( Eq )

-- Needed for pretty-printing
instance Show Expr where
  show (Var i)              = "Var " ++ show i
  show (Lam ((v, t), body)) = "Lambda (" ++ show v ++ ", " ++ show t ++ ") " ++ exprShow body
  show (App (lam, param))   = "App " ++ exprShow lam ++ " " ++ exprShow param

-- ADT for type values
data Type = TInt
  | TBool
  | TFun (Type, Type)
  deriving (Eq)

-- Needed for pretty-printing
instance Show Type where
  show TInt  = "TInt"
  show TBool = "TBool"
  show (TFun (param, body)) = "TFun (" ++ show param ++ ") (" ++ show body ++ ")"

-- ADT for STLC result values
data Val = VInt Int | VClos Int Expr Environment
    deriving (Eq, Show)

----------------------
-- Helper functions --
----------------------

exprShow :: Expr -> String
exprShow e
  | singleton e = show e
  | otherwise = "(" ++ show e ++ ")"
   
singleton :: Expr -> Bool
singleton (Var _) = True
singleton _       = False
