module SmallCheck.SpecArith where

import Arithmetic.Grammar (Expr (..))
import qualified Arithmetic.Interp1 as I1
import qualified Arithmetic.Interp2 as I2
import qualified Arithmetic.InterpFaulty1 as IF1

import Test.SmallCheck ( smallCheck )

import Test.Hspec ()

import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Runner as H

import Test.Hspec.SmallCheck ()

-- property for equivalent interpreters
prop_correct_interp :: Expr -> Bool 
prop_correct_interp expr = I1.interp expr == I2.interp expr

-- property for non-equivalent interpreters
prop_faulty_interp :: Expr -> Bool 
prop_faulty_interp expr = I1.interp expr == IF1.interp expr

-- Main driver code
main :: IO ()
main = do
    putStrLn "Checking correct arithmetic interpretation:"
    smallCheck 3 prop_correct_interp

    putStrLn "Checking faulty arithmetic interpretation:"
    smallCheck 3 prop_faulty_interp    

    return ()

-- main :: IO ()
-- main = H.hspecWith (H.defaultConfig { H.configSmallCheckDepth = 3} ) spec

-- spec :: Spec
-- spec = do
--     describe "QuickCheck Arithmetic Testing:" $ do
--         it "Equivalent interpreters:" $
--             property prop_correct_interp
--         it "Non-equivalent interpreters:" $
--             property prop_faulty_interp