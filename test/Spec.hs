import Arithmetic.Grammar ()
import qualified Arithmetic.Interp1 as I1
import qualified Arithmetic.Interp2 as I2
import qualified Arithmetic.InterpFaulty1 as IF1

import BTrees.Grammar ( Tree(Leaf, Branch) )
import qualified BTrees.Interp1 as B1
import qualified BTrees.Interp2 as B2
import qualified BTrees.InterpFaulty1 as BF1

import Test.QuickCheck

lb :: Int
lb = 1

rb :: Int
rb = 10

main :: IO ()
main = do
    putStrLn "Generating a random integer:"
    el <- generate $ frequency [(1, choose (1 :: Int, 10 :: Int)), (100, choose (11 :: Int, 20 :: Int)), (0, choose (21 :: Int, 30 :: Int))]
    print el
    return ()
