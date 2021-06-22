import qualified QuickCheck.SpecArith as QA
import qualified QuickCheck.SpecBools as QB 
import qualified QuickCheck.SpecCond as QC 

import qualified SmallCheck.SpecArith as SA 
import qualified SmallCheck.SpecBools as SB 
import qualified SmallCheck.SpecCond as SC

-- loop for executing a test suite 10 times
loop :: Int -> IO () -> IO ()
loop 0 test = test 
loop n test = 
    do 
        loop (n - 1) test

        -- Formatting logs
        putStrLn $ "\nRunning iteration " ++ show n ++ ":"
        putStrLn "--------------------\n"

        -- Executing a test suite
        test 

-- Main driver code
main :: IO ()
main = loop 10 SA.main