module QuickCheck.SpecSpaces where

import Spaces.Grammar
import Spaces.Generator
import qualified Spaces.Interp as I1

import Test.QuickCheck

prop_correct_interp :: Expr -> Bool 
prop_correct_interp expr = I1.interp I1.sampleEnv expr == I1.interp I1.sampleEnv expr

main :: IO ()
main = do
    putStrLn "Checking correct interpretation of SLC:"
    quickCheck prop_correct_interp
    