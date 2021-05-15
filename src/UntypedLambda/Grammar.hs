module UntypedLambda.Grammar where

type Var = String

data Val = VInt Int | VBool Bool | VClos String Expr Environment
  deriving ( Eq, Show )

type Environment = [(String, Val)]

-- Reference untyped lambda calculus
-- taken from Types & Programming Languages
-- book by Benjamin C. Pierce
data Expr = Id Var
          | Lambda String Expr
          | App Expr Expr
          deriving (Eq)

instance Show Expr where
    show (Id x) = x
    show (Lambda arg body) = "\\" ++ arg ++ ".(" ++ show body ++ ")"
    show (App fun param)   = "(" ++ show fun ++ ") (" ++ show param ++ ")"


--  Sample variables
sampleEnv :: Environment
sampleEnv = 
    [
        ("x", VInt 1),
        ("y", VInt 2),
        ("tru", VClos "t" (Lambda "f" (Id "t")) []),
        ("fls", VClos "t" (Lambda "f" (Id "t")) []),
        -- ("pair", VClos "f" (Lambda ) []),
        ("and", VClos "b" (Lambda "c" (App (App (Id "b") (Id "c")) (Id "fls"))) []),
        ("fst", VClos "a" (Lambda "b" (Id "a")) []),
        ("snd", VClos "a" (Lambda "b" (Id "b")) [])
    ] 

sample1 :: Expr
sample1 = Id "x"

sample2 :: Expr
sample2 = Lambda "x" (Id "x")

sample3 :: Expr 
sample3 = App (Lambda "x" (Id "x")) (Id "y")