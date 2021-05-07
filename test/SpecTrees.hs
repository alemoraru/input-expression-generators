import BTrees.Grammar
import qualified BTrees.Interp1 as B1
import qualified BTrees.Interp2 as B2
import qualified BTrees.InterpFaulty1 as BF1

import Arithmetic.Grammar
import qualified Arithmetic.Interp1 as I1
import qualified Arithmetic.Interp2 as I2
import qualified Arithmetic.InterpFaulty1 as IF1

import Test.QuickCheck

prop_interp_trees :: Tree Int -> Bool
prop_interp_trees tree = B1.interp tree == B2.interp tree

prop_faulty_tree :: Tree Int -> Bool 
prop_faulty_tree tree = B1.interp tree == BF1.interp tree

prop_interp_arithmetic :: Expr -> Bool 
prop_interp_arithmetic expr = undefined 

prop_interp_faulty_arithmetic :: Expr -> Bool 
prop_interp_faulty_arithmetic expr = undefined 

main :: IO ()
main = do
    putStrLn "Checking interp trees correct: "
    quickCheck prop_interp_trees

    putStrLn "Checking interp tree incorrect: "
    quickCheck prop_faulty_tree

    return ()
