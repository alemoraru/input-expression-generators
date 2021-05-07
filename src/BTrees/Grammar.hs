module BTrees.Grammar where

import Test.QuickCheck
import Control.Monad

data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show (Leaf x)     = show x
  show (Branch l r) = show l ++ show r

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized arbTree

arbTree 0 = fmap Leaf arbitrary
arbTree n = frequency
  [ (1, fmap Leaf arbitrary)
  , (8, liftM2 Branch (arbTree (n `div` 2))
                      (arbTree (n `div` 2)))]