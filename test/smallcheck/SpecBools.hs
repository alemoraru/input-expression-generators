module SmallCheck.SpecBools where

import Booleans.Grammar (Expr (..))
import qualified Booleans.Interp1 as I1
import qualified Booleans.Interp2 as I2
import qualified Booleans.InterpFaulty1 as IF1

import Test.SmallCheck ( smallCheck )
import Test.Hspec ()
import Test.Hspec.SmallCheck ()

-- property for equivalent interpreters
prop_correct_interp :: Expr -> Bool 
prop_correct_interp expr = I1.interp expr == I2.interp expr

-- property for non-equivalent interpreters
prop_faulty_interp :: Expr -> Bool 
prop_faulty_interp expr = I1.interp expr == IF1.interp expr

-- Main driver code
main :: IO ()
main = do
    putStrLn "Checking correct booleans interpretation:"
    smallCheck 4 prop_correct_interp

    putStrLn "Checking faulty booleans interpretation:"
    smallCheck 4 prop_faulty_interp    

    return ()