import Test.Hspec ( hspec, describe, Spec )

import qualified QuickCheck.SpecArith
import qualified QuickCheck.SpecBools
import qualified QuickCheck.SpecCond

-- Main driver code
main :: IO ()
main = hspec spec

-- The tests that will be run 
-- when executing the command "stack test"
spec :: Spec 
spec = do
    describe "QuickArith" QuickCheck.SpecArith.spec 
    describe "QuickBools" QuickCheck.SpecBools.spec
    describe "QuickCond" QuickCheck.SpecCond.spec