module QuickCheck.SpecArith where

import Arithmetic.Grammar (Expr (..))
import qualified Arithmetic.Interp1 as I1
import qualified Arithmetic.Interp2 as I2
import qualified Arithmetic.InterpFaulty1 as IF1

import Test.QuickCheck (quickCheck, Testable (property))

import Test.Hspec

prop_correct_interp :: Expr -> Bool 
prop_correct_interp expr = I1.interp expr == I2.interp expr

prop_faulty_interp :: Expr -> Bool 
prop_faulty_interp expr = I1.interp expr == IF1.interp expr

prop_addition :: Expr -> Expr -> Bool 
prop_addition e1 e2 = I1.interp (Add e1 e2) == I2.interp (Add e1 e2)

prop_incorrect_addition :: Expr -> Expr -> Bool 
prop_incorrect_addition e1 e2 = I2.interp (Add e1 e2) == IF1.interp (Add e1 e2)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "QuickCheck Arithmetic Testing:" $ do
        it "Equivalent interpreters:" $
            property prop_correct_interp
        it "Non-equivalent interpreters:" $
            property prop_faulty_interp