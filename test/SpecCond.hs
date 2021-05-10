import Conditional.Grammar (Expr (..))
import qualified Conditional.Interp1 as I1
import qualified Conditional.Interp2 as I2
import qualified Conditional.InterpFaulty1 as IF1

import Test.QuickCheck

-- Pre-condition basically type-checks input expressions
preConditionInterp :: Expr -> Bool 
preConditionInterp expr = 
    case I1.interp expr [] of 
        Left err -> False 
        Right _  -> True

prop_correct_interp :: Expr -> Property  
prop_correct_interp expr = preConditionInterp expr ==> I1.interp expr [] == I2.interp expr []

prop_faulty_interp :: Expr -> Property 
prop_faulty_interp expr = preConditionInterp expr ==> I1.interp expr [] == IF1.interp expr []

main :: IO ()
main = do
    putStrLn "Testing correct intepretation:"
    quickCheck prop_correct_interp

    putStrLn "Testing incorrect interpretation:"
    quickCheck prop_faulty_interp

    return ()