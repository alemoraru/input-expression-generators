import Arithmetic.Grammar
import qualified Arithmetic.Interp1 as I1
import qualified Arithmetic.Interp2 as I2
import qualified Arithmetic.InterpFaulty1 as IF1

import BTrees.Grammar
import qualified BTrees.Interp1 as B1
import qualified BTrees.Interp2 as B2
import qualified BTrees.InterpFaulty1 as BF1

main :: IO ()
main = do
    -- print (I1.interp (Add (Val 1) (Val 2)))
    print (B1.interp (Branch (Leaf 1) (Leaf 2)))
    print (B2.interp (Branch (Leaf 1) (Leaf 2)))
    print (BF1.interp (Branch (Leaf 1) (Leaf 2)))

    return ()
