import Test.QuickCheck

import SimpleFunctions ( add, mul )

-- QuickCheck properties
prop_com_add :: Int -> Int -> Bool
prop_com_add x y = add x y == add y x

prop_asoc_add :: Int -> Int -> Int -> Bool 
prop_asoc_add x y z = x + (y + z) == (x + y) + z

prop_com_mul :: Int -> Int -> Bool
prop_com_mul x y = mul x y == mul y x

prop_asoc_mul :: Int -> Int -> Int -> Bool 
prop_asoc_mul x y z = x * (y * z) == (x * y) * z

main :: IO ()
main = do
    putStrLn "Checking commutativity of integer addition: "
    quickCheck prop_com_add
    putStrLn "Checking associativity of integer addition: "
    quickCheck prop_asoc_add
    putStrLn "Checking commutativity of integer multiplication"
    quickCheck prop_com_mul
    putStrLn "Checking associativity of integer multiplication: "
    quickCheck prop_asoc_mul
    return ()
