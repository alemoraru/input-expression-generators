module QuickCheck.SpecArith where

import Arithmetic.Grammar (Expr (..))
import qualified Arithmetic.Interp1 as I1
import qualified Arithmetic.Interp2 as I2
import qualified Arithmetic.InterpFaulty1 as IF1

import Test.QuickCheck (quickCheck, Testable (property))

import Test.Hspec ( hspec, describe, it, Spec )

-- property for equivalent interpreters
prop_correct_interp :: Expr -> Bool 
prop_correct_interp expr = I1.interp expr == I2.interp expr

-- property for non-equivalent interpreters
prop_faulty_interp :: Expr -> Bool 
prop_faulty_interp expr = I1.interp expr == IF1.interp expr

-- (quite useless) property for correct addition
prop_addition :: Expr -> Expr -> Bool 
prop_addition e1 e2 = I1.interp (Add e1 e2) == I2.interp (Add e1 e2)

-- (quite useless) property for faulty addition
prop_incorrect_addition :: Expr -> Expr -> Bool 
prop_incorrect_addition e1 e2 = I2.interp (Add e1 e2) == IF1.interp (Add e1 e2)

-- Main driver code
main :: IO ()
main = hspec spec

-- Auxiliary main that uses
-- the hspec package
spec :: Spec
spec = do
    describe "QuickCheck Arithmetic Testing:" $ do
        it "Equivalent interpreters:" $
            property prop_correct_interp
        it "Non-equivalent interpreters:" $
            property prop_faulty_interp