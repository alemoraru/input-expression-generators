module UniformGeneration.SpecCond where

import Conditional.Grammar (Expr (..))
import Conditional.Generator ()
import Conditional.TypeChecker ( typeCheck ) 

import qualified Conditional.Suite.Interp1 as I1
import qualified Conditional.Suite.Interp2 as I2
import qualified Conditional.Suite.InterpFaulty1 as IF1
import qualified Conditional.Suite.InterpFaulty2 as IF2
import qualified Conditional.Suite.InterpFaulty3 as IF3

import Test.QuickCheck
    ( Testable(property), (==>), collect, Property, quickCheck )

import Test.Hspec ( hspec, describe, it, Spec )

-- Pre-condition that "type-checks" input expressions
preConditionInterp :: Expr -> Bool 
preConditionInterp expr = 
    case typeCheck expr [] of 
        Left err -> False 
        Right _  -> True

-- Function that computes the depth of an expression
depth :: Expr -> Integer 
depth (EInt _)         = 1
depth (EBool _)        = 1
depth (Id _)           = 1
depth (Add (l, r))     = 1 + max (depth l) (depth r)
depth (Mul (l, r))     = 1 + max (depth l) (depth r)
depth (Not e)          = 1 + depth e
depth (Or (l, r))      = 1 + max (depth l) (depth r)
depth (And (l, r))     = 1 + max (depth l) (depth r)
depth (Eq (l, r))      = 1 + max (depth l) (depth r)
depth (Lt (l, r))      = 1 + max (depth l) (depth r)
depth (Gt (l, r))      = 1 + max (depth l) (depth r)
depth (Lambda (s, e))  = 1 + depth e
depth (App (l, r))     = 1 + max (depth l) (depth r)
depth (If (b, (t, f))) = 1 + max (depth b) (max (depth t) (depth f))

-- Property for checking equivalent interpreters
-- Also showcase distribution of values per depth of parse tree
prop_correct_interp :: Expr -> Property  
prop_correct_interp expr = preConditionInterp expr ==> collect (depth expr) $ I1.interp expr [] == I2.interp expr []

-- Property for checking non-equivalent properties
prop_faulty_interp1 :: Expr -> Property  
prop_faulty_interp1 expr = preConditionInterp expr ==> I1.interp expr [] == IF1.interp expr []

-- Property for checking non-equivalent properties
prop_faulty_interp2 :: Expr -> Property 
prop_faulty_interp2 expr = preConditionInterp expr ==> I1.interp expr [] == IF2.interp expr []

-- Property for checking non-equivalent properties
prop_faulty_interp3 :: Expr -> Property 
prop_faulty_interp3 expr = preConditionInterp expr ==> I1.interp expr [] == IF2.interp expr []

-- Main driver code
main :: IO ()
main = do
    putStrLn "Equivalent interpreters:"
    quickCheck prop_correct_interp

    putStrLn "Non-equivalent interpreters (1):"
    quickCheck prop_faulty_interp1

    putStrLn "Non-equivalent interpreters (2):"
    quickCheck prop_faulty_interp2

    putStrLn "Non-equivalent interpreters (3):"
    quickCheck prop_faulty_interp3
    