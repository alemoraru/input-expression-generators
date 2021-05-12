module QuickCheck.SpecBools where

import Booleans.Grammar (Expr (..))
import qualified Booleans.Interp1 as I1
import qualified Booleans.Interp2 as I2
import qualified Booleans.InterpFaulty1 as IF1

import Test.QuickCheck (quickCheck, Testable (property))

import Test.Hspec

prop_correct_interp :: Expr -> Bool 
prop_correct_interp expr = I1.interp expr == I2.interp expr

prop_faulty_interp :: Expr -> Bool 
prop_faulty_interp expr = I1.interp expr == IF1.interp expr

main :: IO ()
main = do
    putStrLn "Testing correct intepretation:"
    quickCheck prop_correct_interp

    putStrLn "Testing incorrect interpretation:"
    quickCheck prop_faulty_interp

    return ()

spec :: Spec
spec = do
    describe "QuickCheck Boolean Testing:" $ do
        it "Equivalent interpreters" $
            property prop_correct_interp
        it "Non-equivalent interpreters:" $
            property prop_faulty_interp