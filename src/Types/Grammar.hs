module Types.Grammar ( Expr ( .. ), Val ( .. ), Type ( .. ), Environment, TEnvironment ) where

data Val = VInt Int | VBool Bool | VClos String Expr Environment
  deriving ( Eq )

instance Show Val where
    show (VInt x)  = show x
    show (VBool x) = show x
    show (VClos arg body env) = "\\" ++ arg ++ ".(" ++ show body ++ ")"

data Type = TInt | TBool | TClos Type Type
  deriving ( Eq )
  
instance Show Type where
  show TInt  = "type: Int"
  show TBool = "type: Bool"
  show (TClos argTy retTy) = "from " ++ show argTy ++ " to " ++ show retTy 

type Environment  = [(String, Val)]
type TEnvironment = [(String, Type)]
  
data Expr =
  -- basic building blocks
  EInt Int | EBool Bool | Id String
  -- basic operations on ints
  | Add Expr Expr | Mul Expr Expr
  -- basic operations on booleans
  | Not Expr | Or Expr Expr | And Expr Expr
  -- comparisons
  | Eq Expr Expr | Lt Expr Expr | Gt Expr Expr
  -- functions
  | Lambda (String, Type) Expr | App Expr Expr
  -- conditionals
  | If Expr Expr Expr
  deriving ( Eq )
  
instance Show Expr where
  show (EInt x)  = show x
  show (EBool b) = show b
  show (Id s)    = s

  show (Add left right) = "(" ++ show left ++ " + " ++ show right ++ ")" 
  show (Mul left right) = "(" ++ show left ++ " * " ++ show right ++ ")"

  show (Not e) = "(not " ++ show e ++ ")"
  show (Or left right)  = "(" ++ show left ++ " || " ++ show right ++ ")"
  show (And left right) = "(" ++ show left ++ " && " ++ show right ++ ")"
  
  show (Eq left right) = "(" ++ show left ++ " == " ++ show right ++ ")"
  show (Lt left right) = "(" ++ show left ++ " < " ++ show right ++ ")"
  show (Gt left right) = "(" ++ show left ++ " > " ++ show right ++ ")"

  show (Lambda s e) = "(\\ (" ++ fst s ++ ") " ++ show e ++ ")"
  show (App f e)    = "(" ++ show f ++ " " ++ show e ++ ")"

  show (If i t e) = "(if " ++ show i ++ " then " ++ show t ++ " else " ++ show e ++ ")"
