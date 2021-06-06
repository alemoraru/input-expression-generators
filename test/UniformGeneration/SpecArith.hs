module UniformGeneration.SpecArith where

import Arithmetic.Generator
    ( Expr,
      interpC1,
      interpC2,
      interpF1,
      interpF2,
      interpF3,
      interpF4 )

import Test.QuickCheck ( quickCheck )

-- property for equivalent interpreters
prop_correct_interp :: Expr -> Bool 
prop_correct_interp expr = interpC1 expr == interpC2 expr

-- property for non-equivalent interpreters
prop_faulty_interp1 :: Expr -> Bool 
prop_faulty_interp1 expr = interpC1 expr == interpF1 expr

-- property for non-equivalent interpreters
prop_faulty_interp2 :: Expr -> Bool 
prop_faulty_interp2 expr = interpC1 expr == interpF2 expr

-- property for non-equivalent interpreters
prop_faulty_interp3 :: Expr -> Bool 
prop_faulty_interp3 expr = interpC1 expr == interpF3 expr

-- property for non-equivalent interpreters
prop_faulty_interp4 :: Expr -> Bool 
prop_faulty_interp4 expr = interpC1 expr == interpF4 expr

-- Main driver code
main :: IO ()
main = do
    putStrLn "Equivalent interpreters:"
    quickCheck prop_correct_interp

    putStrLn "Non-equivalent interpreters (1):"
    quickCheck prop_faulty_interp1

    putStrLn "Non-equivalent interpreters (2):"
    quickCheck prop_faulty_interp2

    putStrLn "Non-equivalent interpreters (3):"
    quickCheck prop_faulty_interp3

    putStrLn "Non-equivalent interpreters (4):"
    quickCheck prop_faulty_interp4