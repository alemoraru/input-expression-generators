module Types.Generator where

import Types.Grammar
import Test.QuickCheck

import Control.Monad

data GenRule = GenVar
             | GenLam
             | GenApp
             deriving (Show)

arbRule :: Gen GenRule
arbRule = frequency 
    [
        (1, return GenVar),
        (1, return GenLam), 
        (1, return GenApp)
    ]

instance Arbitrary Expr where
    arbitrary = do
        genTy <- arbitrary :: Gen Type
        genVar genTy sampleTypeEnvironment

instance Arbitrary Type where
    arbitrary = getRandomType 2

instance Arbitrary GenRule where
    arbitrary = arbRule

-- Get a random variable from the provided environment
getRandomVar :: TEnvironment -> Gen Expr 
getRandomVar nv = 
    do 
        (var, ty) <- elements nv
        return (Id var)

-- Generate a random type
getRandomType :: Int -> Gen Type 
getRandomType 0 = oneof [return TInt, return TBool] 
getRandomType n = 
    do
        frequency 
            [
                (1, oneof [return TInt, return TBool]),
                (1, funTy) -- need change
            ]
        where funTy = 
                do
                    argTy <- getRandomType (n - 1)
                    retTy <- getRandomType (n - 1)
                    return (TClos argTy retTy)

-- Variable generation rule
genVar :: Type -> TEnvironment -> Gen Expr 
genVar reqTy nv = 
    case filter (\(str, ty) -> ty == reqTy) nv of
        []    -> return (Id "not found")
        reqNv -> getRandomVar reqNv

-- Lambda generation rule
genLam :: Type -> TEnvironment -> Gen Expr
genLam = undefined 

-- Function application generation rule
genApp :: Type -> TEnvironment -> Gen Expr 
genApp = undefined 

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
    ("or", TClos TBool (TClos TBool TBool)),
    ("isNeg", TClos TInt TBool),
    ("isZero", TClos TInt TBool),
    ("isTrue", TClos TBool TBool),
    ("abs", TClos TInt TInt),
    ("addOne", TClos TInt TInt)
  ]
  