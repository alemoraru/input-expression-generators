import Arithmetic.Grammar (Expr (..))
import qualified Arithmetic.Interp1 as I1
import qualified Arithmetic.Interp2 as I2
import qualified Arithmetic.InterpFaulty1 as IF1

import Test.QuickCheck (quickCheck)

prop_correct_interp :: Expr -> Bool 
prop_correct_interp expr = I1.interp expr == I2.interp expr

prop_faulty_interp :: Expr -> Bool 
prop_faulty_interp expr = I2.interp expr /= IF1.interp expr -- need check on correct examples

main :: IO ()
main = do
    putStrLn "Testing correct intepretation:"
    quickCheck prop_correct_interp

    putStrLn "Testing incorrect interpretation:"
    quickCheck prop_faulty_interp

    return ()
