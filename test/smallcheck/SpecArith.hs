module SmallCheck.SpecArith where

import Arithmetic.Grammar (Expr (..))
import qualified Arithmetic.Interp1 as I1
import qualified Arithmetic.Interp2 as I2
import qualified Arithmetic.InterpFaulty1 as IF1
import qualified Arithmetic.InterpFaulty2 as IF2
import qualified Arithmetic.InterpFaulty3 as IF3
import qualified Arithmetic.InterpFaulty4 as IF4

import Test.SmallCheck ( smallCheck )

-- property for equivalent interpreters
prop_correct_interp :: Expr -> Bool 
prop_correct_interp expr = I1.interp expr == I2.interp expr

-- property for non-equivalent interpreters
prop_faulty_interp1 :: Expr -> Bool 
prop_faulty_interp1 expr = I1.interp expr == IF1.interp expr

-- property for non-equivalent interpreters
prop_faulty_interp2 :: Expr -> Bool 
prop_faulty_interp2 expr = I1.interp expr == IF2.interp expr

-- property for non-equivalent interpreters
prop_faulty_interp3 :: Expr -> Bool 
prop_faulty_interp3 expr = I1.interp expr == IF3.interp expr

-- property for non-equivalent interpreters
prop_faulty_interp4 :: Expr -> Bool 
prop_faulty_interp4 expr = I1.interp expr == IF4.interp expr

-- Main driver code
main :: IO ()
main = do
    putStrLn "Checking correct arithmetic interpretation:"
    smallCheck 4 prop_correct_interp

    putStrLn "Checking faulty arithmetic interpretation (1):"
    smallCheck 4 prop_faulty_interp1 

    putStrLn "Checking faulty arithmetic interpretation (2):"
    smallCheck 4 prop_faulty_interp2 

    putStrLn "Checking faulty arithmetic interpretation (3):"
    smallCheck 4 prop_faulty_interp3  

    putStrLn "Checking faulty arithmetic interpretation (4):"
    smallCheck 4 prop_faulty_interp4   