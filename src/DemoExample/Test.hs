module DemoExample.Test where

import DemoExample.Grammar ( Expr (..), evil, eval )
import Test.QuickCheck (quickCheck)
import Test.SmallCheck (smallCheck)

prop_faulty :: Expr -> Bool 
prop_faulty e = eval e == evil e

main :: IO ()
main = do
    putStrLn "Uniform generation:"
    quickCheck prop_faulty

    putStrLn "SmallCheck:"
    smallCheck 4 prop_faulty
