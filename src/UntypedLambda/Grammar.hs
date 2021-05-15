module UntypedLambda.Grammar where

type Var = String

-- Reference untyped lambda calculus
-- taken from Types & Programming Languages
-- book by Benjamin C. Pierce
data Expr = Id Var
          | Lambda String Expr
          | App Expr Expr
          deriving (Eq)

instance Show Expr where
    show (Id x) = x
    show (Lambda arg body) = "\\" ++ arg ++ "." ++ show body
    show (App fun param)   = "(" ++ show fun ++ ") (" ++ show param ++ ")"

sample1 :: Expr
sample1 = Id "x"

sample2 :: Expr
sample2 = Lambda "x" (Id "x")

sample3 :: Expr 
sample3 = App (Lambda "x" (Id "x")) (Id "y")