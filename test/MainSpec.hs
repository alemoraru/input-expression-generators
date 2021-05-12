import Test.Hspec

import qualified QuickCheck.SpecArith
import qualified QuickCheck.SpecBools
import qualified QuickCheck.SpecCond

import qualified SmallCheck.SpecArith

main :: IO ()
main = hspec spec

spec :: Spec 
spec = do
    describe "QuickArith" QuickCheck.SpecArith.spec 
    describe "QuickBools" QuickCheck.SpecBools.spec
    describe "QuickCond" QuickCheck.SpecCond.spec