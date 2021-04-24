import Arithmetic.Grammar
import qualified Arithmetic.Interp1 as I1
import qualified Arithmetic.Interp2 as I2
import qualified Arithmetic.InterpFaulty1 as IF1

main :: IO ()
main = do
    print (I1.interp (Add (Val 1) (Val 2)))
    return ()
