import Conditional.Grammar (Expr (..))
import qualified Conditional.Interp1 as I1
import qualified Conditional.Interp2 as I2
import qualified Conditional.InterpFaulty1 as IF1

import Test.QuickCheck

prop_correct_interp :: Expr -> Bool 
prop_correct_interp expr = I1.interp expr [] == I2.interp expr []

prop_faulty_interp :: Expr -> Bool 
prop_faulty_interp expr = I1.interp expr [] == IF1.interp expr []

main :: IO ()
main = do
    putStrLn "Testing correct intepretation:"
    quickCheck prop_correct_interp

    putStrLn "Testing incorrect interpretation:"
    quickCheck prop_faulty_interp

    return ()