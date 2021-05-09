module BTrees.Grammar where

import Test.QuickCheck
import Control.Monad

import qualified Data.Tree as Pretty

data Tree a = Leaf a | Branch (Tree a) a (Tree a) deriving (Eq,Ord,Show) 

toDataTree :: Tree a -> Pretty.Tree a
toDataTree (Leaf a) = Pretty.Node a []
toDataTree (Branch cs b ds) = Pretty.Node b [toDataTree cs, toDataTree ds]

d :: Tree [Char]
d = Branch (Branch (Leaf "111") "11" (Leaf "112")) 
            "1"
           (Branch (Leaf "121") "12" (Leaf "122"))

e :: Pretty.Tree [Char]
e = toDataTree d
f :: IO ()
f = putStrLn $ Pretty.drawTree e

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized arbTree

arbTree 0 = fmap Leaf arbitrary
arbTree n = frequency
  [ (1, fmap Leaf arbitrary)
  , (2, liftM3 Branch (arbTree (n `div` 2))
                      arbitrary 
                      (arbTree (n `div` 2)))]