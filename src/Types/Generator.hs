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
        genRule <- arbitrary :: Gen GenRule
        arbExpr genRule

arbExpr :: GenRule -> Gen Expr 
arbExpr GenVar = do
    genTy <- arbitrary :: Gen Type
    genVar genTy sampleTypeEnvironment
arbExpr GenApp = genApp sampleTypeEnvironment
arbExpr GenLam = undefined

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

-- Get the argument type of a lambda
getArgType :: Type -> Gen Type 
getArgType (TClos argTy _) = return argTy

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
genApp :: TEnvironment -> Gen Expr 
genApp nv = 
    do
        lamTy   <- getRandomType 2
        pLamTy  <- getArgType lamTy
        do 
            lamExpr   <- genVar lamTy nv
            paramExpr <- genVar pLamTy nv
            return $ App lamExpr paramExpr
        
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

sampleEnvironment :: Environment  
sampleEnvironment = 
  [
    ("tru", VBool True),
    ("fls", VBool False),
    ("zero", VInt 0),
    ("one", VInt 1)
    -- ("not", TClos TBool TBool),
    -- ("and", TClos TBool (TClos TBool TBool)),
    -- ("or", TClos TBool (TClos TBool TBool)),
    -- ("isNeg", TClos TInt TBool),
    -- ("isZero", TClos TInt TBool),
    -- ("isTrue", TClos TBool TBool),
    -- ("abs", TClos TInt TInt),
    -- ("addOne", TClos TInt TInt)
  ]

