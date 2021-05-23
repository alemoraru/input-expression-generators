{-# LANGUAGE GADTs #-}

module Spaces.Definition where

import Data.Maybe

data Nat = Z | Suc Nat deriving (Show, Eq)
data ListNat = Nill | Cons Nat ListNat deriving (Show, Eq)

sizeNat :: Nat -> Int
sizeNat Z       = 1
sizeNat (Suc z) = 1 + sizeNat z

sizeListNat :: ListNat -> Int
sizeListNat Nill        = 1
sizeListNat (Cons z zs) = 1 + sizeNat z + sizeListNat zs

-- Definition of a GADT Space to represent ADTs
data Space a where
    Empty :: Space a
    Pure  :: a -> Space a
    (:+:) :: Space a -> Space a -> Space a
    (:*:) :: Space a -> Space b -> Space (a,b)
    Pay   :: Space a -> Space a
    (:$:) :: (a -> b) -> Space a -> Space b

-- Produce a space of all applications of functions to params
(<∗>) :: Space (a -> b) -> Space a -> Space b
s1 <∗> s2 = (\(f ,a) -> f a) :$: (s1 :*: s2)

-- Space for Nats
spaceNat :: Space Nat
spaceNat = Pay (Pure Z :+: (Suc :$: spaceNat))

-- Space for list of Nats
spaceListNat :: Space ListNat
spaceListNat = Pay (Pure Nill :+: (Cons :$: spaceNat <∗> spaceListNat))

-- Data type for finite sets
data FinSet a = EmptySet
              | SingletonSet a
              | DisjointSet (FinSet a) (FinSet a)
              | CartesianSet (FinSet a) (FinSet a)

instance Show a => Show (FinSet a) where
    show EmptySet           = "{}"
    show (SingletonSet x)   = "{" ++ show x ++ "}"
    show (DisjointSet x y)  = show x ++ " U " ++ show y
    show (CartesianSet x y) = show x ++ " x " ++ show y

-- Comute the cardinality of a finite set
card :: FinSet a -> Integer
card EmptySet           = 0
card (SingletonSet x)   = 1
card (DisjointSet x y)  = card x + card y
card (CartesianSet x y) = card x * card y

-- Indexing function on the finite set type
indexFin :: FinSet a -> Integer -> Maybe a
indexFin EmptySet           i = Nothing
indexFin (SingletonSet x)   i | i == 0     = Just x
                              | otherwise  = Nothing
indexFin (DisjointSet x y)  i | i < card x = indexFin x i
                              | otherwise  = indexFin y (i - card x)
indexFin (CartesianSet x y) i = undefined --(indexFin x (i `div` card y), indexFin y (i `mod` card y))

-- extracts the finite set of values of a given size k from a space
sized :: Space a -> Int -> FinSet a
sized Empty k     = EmptySet
sized (Pure a) k  | k == 0    = SingletonSet a
                  | otherwise = EmptySet
sized (Pay a) k   | k == 0    = EmptySet
                  | otherwise = sized a (k - 1)
sized (a :+: b) k = DisjointSet (sized a k) (sized b k)
sized (a :*: b) k = EmptySet -- needs change
                    where
                        elements    = [CartesianSet (sized a k1) (sized a k2) | k1 <- [0..k], k2 <- [0..k], k1 + k2 == k]
                        setElememts = foldr DisjointSet EmptySet elements
sized (f :$: a) k = EmptySet -- needs change
                    where
                        setA        = sized a k
                        elements    = catMaybes [indexFin setA i | i <- [0..(card setA)]]
                        setElements = foldr (DisjointSet . SingletonSet) EmptySet elements

-- Indexing function on spaces
indexSized :: Space a -> Int -> Integer -> Maybe a
indexSized s k i = indexFin (sized s k) i


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

set123 :: FinSet Int 
set123 = DisjointSet (SingletonSet 1) (DisjointSet (SingletonSet 2) (SingletonSet 3))

set89 :: FinSet Int 
set89 = DisjointSet (SingletonSet 8) (SingletonSet 9)