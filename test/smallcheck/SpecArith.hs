module SmallCheck.SpecArith where

import Arithmetic.Grammar (Expr (..))
import qualified Arithmetic.Interp1 as I1
import qualified Arithmetic.Interp2 as I2
import qualified Arithmetic.InterpFaulty1 as IF1

import Test.SmallCheck

prop_correct_interp :: Expr -> Bool 
prop_correct_interp expr = I1.interp expr == I2.interp expr

prop_faulty_interp :: Expr -> Bool 
prop_faulty_interp expr = I1.interp expr == IF1.interp expr

main :: IO ()
main = do
    putStrLn "Checking correct arithmetic interpretation:"
    smallCheck 3 prop_correct_interp

    putStrLn "Checking faulty arithmetic interpretation:"
    smallCheck 3 prop_faulty_interp    

    return ()