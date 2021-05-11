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

-- Property for checking equivalent interpreters
-- Also showcase distribution of values per depth of parse tree
prop_correct_interp :: Expr -> Property  
prop_correct_interp expr = preConditionInterp expr ==> collect (depth expr) $ I1.interp expr [] == I2.interp expr []

-- Property for checking non-equivalent properties
-- Also showcase distribution of values per depth of parse tree
prop_faulty_interp :: Expr -> Property 
prop_faulty_interp expr = preConditionInterp expr ==> collect (depth expr) $ I1.interp expr [] == IF1.interp expr []

depth :: Expr -> Integer 
depth (EInt _)  = 1
depth (EBool _) = 1
depth (Id _)    = 1
depth (Add l r) = 1 + max (depth l) (depth r)
depth (Mul l r) = 1 + max (depth l) (depth r)
depth (Not e)   = 1 + depth e
depth (Or l r)  = 1 + max (depth l) (depth r)
depth (And l r) = 1 + max (depth l) (depth r)
depth (Eq l r)  = 1 + max (depth l) (depth r)
depth (Lt l r)  = 1 + max (depth l) (depth r)
depth (Gt l r)  = 1 + max (depth l) (depth r)
depth (Lambda _ e) = 1 + depth e
depth (App l r)    = 1 + max (depth l) (depth r)
depth (If b t f)   = 1 + max (depth b) (max (depth t) (depth f))

main :: IO ()
main = do
    putStrLn "Testing correct intepretation:"
    quickCheck prop_correct_interp

    putStrLn "Testing incorrect interpretation:"
    quickCheck prop_faulty_interp

    return ()