module QuickCheck.SpecSpaces where

import Spaces.Grammar ( Expr )
import Spaces.Generator ()
import qualified Spaces.Interp as I1

import Test.QuickCheck ( quickCheck )

-- property that checks correct interpretation
prop_correct_interp :: Expr -> Bool 
prop_correct_interp expr = I1.interp I1.sampleEnv expr == I1.interp I1.sampleEnv expr

-- Main driver code
main :: IO ()
main = do
    putStrLn "Checking correct interpretation of SLC:"
    quickCheck prop_correct_interp
    