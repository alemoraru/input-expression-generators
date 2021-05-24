module SmallCheck.SpecCond where

import Conditional.Grammar (Expr (..), Environment, Val (..))
import qualified Conditional.Interp1 as I1
import qualified Conditional.Interp2 as I2
import qualified Conditional.InterpFaulty1 as IF1

import Test.SmallCheck

import Test.Hspec

import Test.Hspec.SmallCheck

prop_correct_interp :: Expr -> Bool 
prop_correct_interp expr = I1.interp expr testEnvironment == I2.interp expr testEnvironment

prop_faulty_interp :: Expr -> Bool 
prop_faulty_interp expr = I1.interp expr testEnvironment == IF1.interp expr testEnvironment

main :: IO ()
main = do
    putStrLn "Checking correct booleans interpretation:"
    smallCheck 3 prop_correct_interp

    putStrLn "Checking faulty booleans interpretation:"
    smallCheck 3 prop_faulty_interp    

    return ()

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
