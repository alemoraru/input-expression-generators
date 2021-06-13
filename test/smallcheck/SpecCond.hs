module SmallCheck.SpecCond where

import Conditional.Grammar (Expr (..), Environment, Val (..))
import qualified Conditional.Interp1 as I1
import qualified Conditional.Interp2 as I2
import qualified Conditional.InterpFaulty1 as IF1
import qualified Conditional.InterpFaulty2 as IF2
import qualified Conditional.InterpFaulty3 as IF3

import Test.SmallCheck

import Test.Hspec ()

import Test.Hspec.SmallCheck ()

-- property for equivalent interpreters
prop_correct_interp :: Expr -> Bool 
prop_correct_interp expr = I1.interp expr testEnvironment == I2.interp expr testEnvironment

-- property for non-equivalent interpreters
prop_faulty_interp1 :: Expr -> Bool 
prop_faulty_interp1 expr = I1.interp expr testEnvironment == IF1.interp expr testEnvironment

-- property for non-equivalent interpreters
prop_faulty_interp2 :: Expr -> Bool 
prop_faulty_interp2 expr = I1.interp expr testEnvironment == IF2.interp expr testEnvironment

-- property for non-equivalent interpreters
prop_faulty_interp3 :: Expr -> Bool 
prop_faulty_interp3 expr = I1.interp expr testEnvironment == IF3.interp expr testEnvironment

-- main driver code
main :: IO ()
main = do
    putStrLn "Checking correct conditional interpretation:"
    smallCheck 5 prop_correct_interp

    putStrLn "Checking faulty conditional interpretation (1):"
    smallCheck 5 prop_faulty_interp1

    putStrLn "Checking faulty conditional interpretation (2):"
    smallCheck 5 prop_faulty_interp2

    putStrLn "Checking faulty conditional interpretation (3):"
    smallCheck 5 prop_faulty_interp3            

    return ()

-- An environment that is used for testing
testEnvironment :: Environment 
testEnvironment = 
  [
    ("zero", VInt 0),
    ("one", VInt 1),
    ("two", VInt 2),
    ("three", VInt 3),
    ("tru", VBool True),
    ("fls", VBool False)
  ]
