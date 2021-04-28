import BTrees.Grammar
import qualified BTrees.Interp1 as B1
import qualified BTrees.Interp2 as B2
import qualified BTrees.InterpFaulty1 as BF1

import Test.QuickCheck

prop_interp_trees :: Tree Int -> Bool
prop_interp_trees tree = B1.interp tree == B2.interp tree

prop_faulty_tree :: Tree Int -> Bool 
prop_faulty_tree tree = B1.interp tree == BF1.interp tree

main :: IO ()
main = do
    putStrLn "Checking interp trees correct: "
    quickCheck prop_interp_trees

    putStrLn "Checking interp tree incorrect: "
    quickCheck prop_faulty_tree

    return ()
