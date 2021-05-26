{-# LANGUAGE GADTs #-}

module Spaces.Definition where

import Data.Maybe

import System.Random

import qualified Test.QuickCheck as QC

data Nat = Zero | Suc Nat deriving (Show, Eq)
data ListNat = Nil | Cons Nat ListNat deriving (Show, Eq)

-- Count the number of constructors in a Nat expression
sizeNat :: Nat -> Int
sizeNat Zero    = 1
sizeNat (Suc z) = 1 + sizeNat z

-- Count the number of constructors in a ListNat expression
sizeListNat :: ListNat -> Int
sizeListNat Nil         = 1
sizeListNat (Cons z zs) = 1 + sizeNat z + sizeListNat zs

-- Definition of a GADT Space to represent ADTs
data Space a where
    Empty :: Space a
    Pure  :: a -> Space a
    (:+:) :: Space a -> Space a -> Space a
    (:*:) :: Space a -> Space a -> Space (a, a)
    Pay   :: Space a -> Space a
    (:$:) :: (a -> b) -> Space a -> Space b

-- -- Produce a space of all applications of functions to params
-- (<∗>) :: Space (a -> b) -> Space a -> Space b
-- s1 <∗> s2 = (\(f ,a) -> f a) :$: (s1 :*: s2)

-- -- Space for Nats
-- spaceNat :: Space Nat
-- spaceNat = Pay (Pure Zero :+: (Suc :$: spaceNat))

-- -- Space for list of Nats
-- spaceListNat :: Space ListNat
-- spaceListNat = Pay (Pure Nil :+: (Cons :$: spaceNat <∗> spaceListNat))

-- Data type for finite sets
data Set a where
    EmptySet     :: Set a
    SingletonSet :: a -> Set a
    DisjointSet  :: Set a -> Set a -> Set a
    CartesianSet :: Set a -> Set a -> Set (a, a) -- TODO: needs check
    FmapSet      :: (a -> b) -> Set a -> Set b -- TODO: needs check

instance Show a => Show (Set a) where
    show EmptySet           = "{}"
    show (SingletonSet x)   = "{" ++ show x ++ "}"
    show (DisjointSet x y)  = show x ++ " U " ++ show y
    show (CartesianSet x y) = "X"    -- tODO: needs fix --> show x ++ " X " ++ show y
    show (FmapSet f x)      = "fmap" -- TODO: needs fix

-- Comute the cardinality of a finite set
card :: Set a -> Integer
card EmptySet           = 0
card (SingletonSet x)   = 1
card (DisjointSet x y)  = card x + card y
card (CartesianSet x y) = card x * card y
card (FmapSet f x)      = card x

-- Indexing function on the finite set type
indexSet :: Set a -> Integer -> Maybe a -- TODO: needs check on return type
indexSet EmptySet           i = Nothing
indexSet (SingletonSet x)   i | i == 0     = Just x
                              | otherwise  = Nothing
indexSet (DisjointSet x y)  i | i < card x = indexSet x i
                              | otherwise  = indexSet y (i - card x)
indexSet (CartesianSet x y) i =
    case (indexSet x (i `div` card y), indexSet y (i `mod` card y)) of
        (Just lVal, Just rVal) -> Just (lVal, rVal)
        _                      -> Nothing
indexSet (FmapSet f x) i      =
    case indexSet x i of
        (Just val) -> Just (f val)
        _          -> Nothing

-- Return a uniformly random integer in the inclusive interval (lo, hi) 
uniformRange :: (Integer, Integer) -> QC.Gen Integer
uniformRange = QC.chooseInteger

-- Uniform sampling from finite sets
uniformSet :: Set a -> QC.Gen a
uniformSet s | card s == 0 = error "empty set"
             | otherwise = do
                    i <- uniformRange (0, card s - 1)
                    do case indexSet s i of
                        Nothing  -> error "something went wrong"
                        Just set -> return set

-- extracts the finite set of values of a given size k from a space
sized :: Space a -> Int -> Set a
sized Empty k     = EmptySet
sized (Pure a) k  | k == 0    = SingletonSet a
                  | otherwise = EmptySet
sized (Pay a) k   | k == 0    = EmptySet
                  | otherwise = sized a (k - 1)
sized (a :+: b) k = DisjointSet (sized a k) (sized b k)
sized (a :*: b) k = setElememts -- TODO: needs check on types
                    where
                        elements    = [CartesianSet (sized a k1) (sized b k2) | k1 <- [0..k], k2 <- [0..k], k1 + k2 == k]
                        setElememts = foldr DisjointSet EmptySet elements
sized (f :$: a) k = FmapSet f (sized a k)

-- Indexing function on spaces
uniformSized :: Space a -> Int -> QC.Gen a
uniformSized s k = uniformSet (sized s k) 

----------------------------------------------------
--            PREDICATE-GUIDED INDEXING           --
----------------------------------------------------

-- Determines whether a given predicate needs to investigate
-- its argument or not in order to produce its result
valid :: (a -> Bool) -> Maybe Bool
valid = undefined

-- The main indexing function
index :: (a -> Bool) -> Space a -> Int -> Integer -> Space a
index p (f :$: a) k i =
    case valid p' of
        Just _  -> f :$: a
        Nothing -> f :$: index p' a k i
        where p' = p . f
index p _ k i = undefined -- needs check for other cases


----------------------------------------------------
--                 EXAMPLE SETS                   --
----------------------------------------------------


-- Set for {1} U {2} U {3}
set1 :: Set Int
set1 = DisjointSet (SingletonSet 1) (DisjointSet (SingletonSet 2) (SingletonSet 3))

-- Set for {8} U {9}
set2 :: Set Int
set2 = DisjointSet (SingletonSet 8) (SingletonSet 9)

-- Set for set1 x set2
set3 :: Set (Int, Int)
set3 = CartesianSet set1 set2