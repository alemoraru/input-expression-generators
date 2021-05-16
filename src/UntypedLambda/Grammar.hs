module UntypedLambda.Grammar where

type Var = String

data Val = VInt Int | VBool Bool | VClos String Expr Environment
  deriving (Eq)

instance Show Val where
    show (VInt x)  = show x
    show (VBool x) = show x
    show (VClos arg body env) = "\\" ++ arg ++ ".(" ++ show body ++ ")"

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
        ("pair", VClos "f" (Lambda "s" (Lambda "b" (Id "b"))) []), -- faulty
        ("and", VClos "b" (Lambda "c" (App (App (Id "b") (Id "c")) (Id "fls"))) []),
        ("fst", VClos "a" (Lambda "b" (Id "a")) []),
        ("snd", VClos "a" (Lambda "b" (Id "b")) [])
    ] 

-- Sample lambda functions
ltrue :: Expr
ltrue = Lambda "t" (Lambda "f" (Id "t"))

lfalse :: Expr 
lfalse = Lambda "t" (Lambda "f" (Id "f"))

land :: Expr
land = Lambda "b" (Lambda "c" (App (App (Id "b") (Id "c")) lfalse))

-- faulty
lpair :: Expr
lpair = Lambda "f" (Lambda "s" (Lambda "b" (App (App (Id "b") (Id "f")) (Id "s")))) 

lfst :: Expr 
lfst = Lambda "p" (Lambda "p" ltrue)

lsnd :: Expr 
lsnd = Lambda "p" (Lambda "p" lfalse)

-- Examples to interp
sample1 :: Expr
sample1 = Id "x"

sample2 :: Expr
sample2 = Lambda "x" (Id "x")

sample3 :: Expr 
sample3 = App (Lambda "x" (Id "x")) (Id "y")