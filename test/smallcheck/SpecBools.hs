module SmallCheck.SpecBools where

import Booleans.Grammar (Expr (..))
import qualified Booleans.Suite.Interp1 as I1
import qualified Booleans.Suite.Interp2 as I2
import qualified Booleans.Suite.InterpFaulty1 as IF1
import qualified Booleans.Suite.InterpFaulty2 as IF2
import qualified Booleans.Suite.InterpFaulty3 as IF3

import Test.SmallCheck ( smallCheck )
import Test.Hspec ()
import Test.Hspec.SmallCheck ()

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

-- Main driver code
main :: IO ()
main = do
    putStrLn "Checking correct booleans interpretation:"
    smallCheck 4 prop_correct_interp

    putStrLn "Checking faulty booleans interpretation (1):"
    smallCheck 4 prop_faulty_interp1  

    putStrLn "Checking faulty booleans interpretation (2):"
    smallCheck 4 prop_faulty_interp2  

    putStrLn "Checking faulty booleans interpretation (3):"
    smallCheck 4 prop_faulty_interp3  

    return ()