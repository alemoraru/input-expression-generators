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
prop_correct_interp expr = I1.interp expr [] == I2.interp expr []

-- property for non-equivalent interpreters
prop_faulty_interp1 :: Expr -> Bool 
prop_faulty_interp1 expr = I1.interp expr [] == IF1.interp expr []

-- property for non-equivalent interpreters
prop_faulty_interp2 :: Expr -> Bool 
prop_faulty_interp2 expr = I1.interp expr [] == IF2.interp expr []

-- property for non-equivalent interpreters
prop_faulty_interp3 :: Expr -> Bool 
prop_faulty_interp3 expr = I1.interp expr [] == IF3.interp expr []

-- property for non-equivalent interpreters
prop_faulty_interp4 :: Expr -> Bool 
prop_faulty_interp4 expr = I1.interp expr [] == IF4.interp expr []

-- property for non-equivalent interpreters
prop_faulty_interp5 :: Expr -> Bool 
prop_faulty_interp5 expr = I1.interp expr [] == IF5.interp expr []

-- property for non-equivalent interpreters
prop_faulty_interp6 :: Expr -> Bool 
prop_faulty_interp6 expr = I1.interp expr [] == IF6.interp expr []

-- property for non-equivalent interpreters
prop_faulty_interp7 :: Expr -> Bool 
prop_faulty_interp7 expr = I1.interp expr [] == IF7.interp expr []

-- property for non-equivalent interpreters
prop_faulty_interp8 :: Expr -> Bool 
prop_faulty_interp8 expr = I1.interp expr [] == IF8.interp expr []

-- property for non-equivalent interpreters
prop_faulty_interp9 :: Expr -> Bool 
prop_faulty_interp9 expr = I1.interp expr [] == IF9.interp expr []

-- main driver code
mainHelper :: Int -> IO ()
mainHelper n = do
    putStrLn "Checking correct conditional interpretation:"
    smallCheck n prop_correct_interp

    putStrLn "Checking faulty conditional interpretation (1):"
    smallCheck n prop_faulty_interp1

    putStrLn "Checking faulty conditional interpretation (2):"
    smallCheck n prop_faulty_interp2

    putStrLn "Checking faulty conditional interpretation (3):"
    smallCheck n prop_faulty_interp3

    putStrLn "Non-equivalent interpreters (4):"
    smallCheck n prop_faulty_interp4

    putStrLn "Non-equivalent interpreters (5):"
    smallCheck n prop_faulty_interp5

    putStrLn "Non-equivalent interpreters (6):"
    smallCheck n prop_faulty_interp6

    putStrLn "Non-equivalent interpreters (7):"
    smallCheck n prop_faulty_interp7

    putStrLn "Non-equivalent interpreters (8):"
    smallCheck n prop_faulty_interp8

    putStrLn "Non-equivalent interpreters (9):"
    smallCheck n prop_faulty_interp9

-- Main driver code
main :: IO ()
main = mainHelper 5            
