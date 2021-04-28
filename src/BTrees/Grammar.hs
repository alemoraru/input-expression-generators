module BTrees.Grammar where

import Test.QuickCheck
import Control.Monad

data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized arbTree

arbTree 0 = liftM Leaf arbitrary
arbTree n = frequency 
  [ (1, liftM Leaf arbitrary)
  , (4, liftM2 Branch (arbTree (n `div` 2))
                      (arbTree (n `div` 2)))]