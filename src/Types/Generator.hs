module Types.Generator where

import Types.Grammar
import Test.QuickCheck

instance Arbitrary Expr where
  arbitrary = undefined

-- Get a random variable from the provided environment
getRandomVar :: TEnvironment -> Gen Expr 
getRandomVar nv = 
    do 
        (var, ty) <- elements nv
        return (Id var)


-- A sample type environment to use for testing purposes
sampleTypeEnvironment :: TEnvironment 
sampleTypeEnvironment = 
  [
    ("tru", TBool),
    ("fls", TBool),
    ("zero", TInt),
    ("one", TInt),
    ("not", TClos TBool TBool),
    ("and", TClos TBool (TClos TBool TBool)),
    ("or", TClos TBool (TClos TBool TBool))
  ]
