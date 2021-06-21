module SmallCheck.SpecCond where

import Conditional.Grammar (Expr (..), Environment, Val (..))
import qualified Conditional.Suite.Interp1 as I1
import qualified Conditional.Suite.Interp2 as I2
import qualified Conditional.Suite.InterpFaulty1 as IF1
import qualified Conditional.Suite.InterpFaulty2 as IF2
import qualified Conditional.Suite.InterpFaulty3 as IF3
import qualified Conditional.Suite.InterpFaulty4 as IF4
import qualified Conditional.Suite.InterpFaulty5 as IF5
import qualified Conditional.Suite.InterpFaulty6 as IF6
import qualified Conditional.Suite.InterpFaulty7 as IF7
import qualified Conditional.Suite.InterpFaulty8 as IF8
import qualified Conditional.Suite.InterpFaulty9 as IF9

import Test.SmallCheck ( smallCheck )

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

-- property for non-equivalent interpreters
prop_faulty_interp4 :: Expr -> Bool 
prop_faulty_interp4 expr = I1.interp expr testEnvironment == IF4.interp expr testEnvironment

-- property for non-equivalent interpreters
prop_faulty_interp5 :: Expr -> Bool 
prop_faulty_interp5 expr = I1.interp expr testEnvironment == IF5.interp expr testEnvironment

-- property for non-equivalent interpreters
prop_faulty_interp6 :: Expr -> Bool 
prop_faulty_interp6 expr = I1.interp expr testEnvironment == IF6.interp expr testEnvironment

-- property for non-equivalent interpreters
prop_faulty_interp7 :: Expr -> Bool 
prop_faulty_interp7 expr = I1.interp expr testEnvironment == IF7.interp expr testEnvironment

-- property for non-equivalent interpreters
prop_faulty_interp8 :: Expr -> Bool 
prop_faulty_interp8 expr = I1.interp expr testEnvironment == IF8.interp expr testEnvironment

-- property for non-equivalent interpreters
prop_faulty_interp9 :: Expr -> Bool 
prop_faulty_interp9 expr = I1.interp expr testEnvironment == IF9.interp expr testEnvironment

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

    putStrLn "Non-equivalent interpreters (4):"
    smallCheck 5 prop_faulty_interp4

    putStrLn "Non-equivalent interpreters (5):"
    smallCheck 5 prop_faulty_interp5

    putStrLn "Non-equivalent interpreters (6):"
    smallCheck 5 prop_faulty_interp6

    putStrLn "Non-equivalent interpreters (7):"
    smallCheck 5 prop_faulty_interp7

    putStrLn "Non-equivalent interpreters (8):"
    smallCheck 5 prop_faulty_interp8

    putStrLn "Non-equivalent interpreters (9):"
    smallCheck 5 prop_faulty_interp9            

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
