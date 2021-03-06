{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Spaces where

import Data.Maybe ()
import System.Random ()
import qualified Test.QuickCheck as QC

import Data.Typeable
import System.IO.Unsafe ( unsafePerformIO )
import Control.Exception

-- Definition of a GADT Space to represent ADTs
data Space a where
    Empty :: Space a
    Pure  :: a -> Space a
    (:+:) :: Space a  -> Space a -> Space a
    (:*:) :: Space a  -> Space b -> Space (a, b)
    Pay   :: Space a  -> Space a
    (:$:) :: (a -> b) -> Space a -> Space b

-- Produce a space of all applications of functions to params
(<∗>) :: Space (a -> b) -> Space a -> Space b
s1 <∗> s2 = (\(f ,a) -> f a) :$: (s1 :*: s2)

-- Data type for finite sets
data Set a where
    EmptySet     :: Set a
    SingletonSet :: a -> Set a
    DisjointSet  :: Set a -> Set a -> Set a
    CartesianSet :: Set a -> Set b -> Set (a, b)
    FmapSet      :: (a -> b) -> Set a -> Set b
    ReplicateSet :: Integer -> a -> Set a 

-- Used for pretty printing Sets (Debugging)
instance Show a => Show (Set a) where
    show EmptySet           = "{}"
    show (SingletonSet x)   = "{" ++ show x ++ "}"
    show (DisjointSet x y)  = show x ++ " U " ++ show y
    show (CartesianSet x y) = "X"    -- Just an example
    show (FmapSet f x)      = "fmap" -- Just an example
    show (ReplicateSet k x) = "replicate " ++ show x ++ show " " ++ show k ++ " times"

-- Comute the cardinality of a finite set
card :: Set a -> Integer
card EmptySet           = 0
card (SingletonSet x)   = 1
card (DisjointSet x y)  = card x + card y
card (CartesianSet x y) = card x * card y
card (FmapSet f x)      = card x
card (ReplicateSet k x) = k

-- Indexing function on the finite set type
indexSet :: Set a -> Integer -> Maybe a
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
indexSet (ReplicateSet k x) i | i < k     = Just x
                              | otherwise = Nothing 

-- Return a uniformly random integer in the inclusive interval (low, high) 
uniformRange :: (Integer, Integer) -> QC.Gen Integer
uniformRange = QC.chooseInteger

-- Uniform sampling from finite sets
uniformSet :: Set a -> QC.Gen a
uniformSet s | card s == 0 = error "empty set"
             | otherwise = do
                    i <- uniformRange (0, card s - 1)
                    case indexSet s i of
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
sized (a :*: b) k = setElememts
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

-- Rejection sampling function to generate a value that satisfies the given predicate
uniformFilter :: (a -> Bool) -> Space a -> Int -> QC.Gen a
uniformFilter p s k = do
    a <- uniformSized s k
    if p a then return a
           else uniformFilter p s k

-- Determine whether a predicate is universally true/false/depends on argument
universal :: (a -> Bool) -> Maybe Bool
universal p = unsafePerformIO $ catch (let x = p undefined in x `seq` pure (Just x)) (\(e :: SomeException) -> pure Nothing)


-- Own defined errors for InspectsFst checking
data InspectsException = FstException | SndException
    deriving Show

instance Exception InspectsException

-- Evaluate a predicate on a pair and see which argument is "inspected" first
inspectsFst :: ((a, b) -> Bool) -> Bool 
inspectsFst p = unsafePerformIO $ catch (let x = p (undefined, undefined) in x `seq` pure True) (\(e :: SomeException) -> pure False)

-- Evaluate a predicate on a pair and see which argument is "inspected" first
inspectsFstExp :: ((a, b) -> Bool) -> Bool 
inspectsFstExp p = unsafePerformIO $ catch (let x = p (throw FstException, throw SndException) in x `seq` pure True) (\case
    FstException -> pure True 
    SndException -> pure False)

-- Improved uniform filter that reduces the space with each failed input
uniform :: (a -> Bool) -> Space a -> Int -> QC.Gen a
uniform p s k = do
    x <- uniformSet (sizedP p s k)
    case x of 
        Left a   -> return a
        Right s' -> uniform p s' k

-- Function used to eliminate products on spaces altogether
(***) :: Space a -> Space b -> Space (a, b)
a *** (b :+: c) = (a :*: b) :+: (a :*: c) -- distributivity law
a *** (b :*: c) = (\((x, y), z) -> (x, (y, z))) :$: ((a :*: b) :*: c) -- associativity law
a *** (Pure x)  = (\y -> (y, x)) :$: a    -- identity law
a *** Empty     = Empty                   -- annihilation law 
a *** (Pay b)   = Pay (a :*: b)           -- lift pay
a *** (f :$: b) = (\(x, y) -> (x, f y)) :$: (a :*: b) -- lift fmap

-- Gives a reduced space if no results are found / results
sizedP :: (a -> Bool) -> Space a -> Int -> Set (Either a (Space a))
sizedP p (f :$: a) k = case universal p' of
    Just False -> ReplicateSet (card $ sized a k) (Right Empty)
    _          -> FmapSet (apply f) (sizedP p' a k)
    where p' = p . f
          apply f x = case x of
              Left x  -> Left  (f x)
              Right a -> Right (f :$: a) 
sizedP p (a :*: b) k = if inspectsFst p
    then sizedP p (swap :$: (b *** a)) k
    else sizedP p (a *** b) k
    where swap (a, b) = (b, a)
sizedP p (a :+: b) k = DisjointSet (rebuild (:+: b) (sizedP p a k)) 
                                   (rebuild (a :+:) (sizedP p b k))
    where 
        rebuild :: (Space a -> Space a) -> Set (Either a (Space a)) -> Set (Either a (Space a))
        rebuild f s = FmapSet (fmap f) s
sizedP p (Pay a) k  | k > 0     = FmapSet (fmap Pay) (sizedP p a (k - 1))
sizedP p (Pure a) 0 | p a       = SingletonSet (Left a)
                    | otherwise = SingletonSet (Right Empty)
sizedP _ _        _ = EmptySet 
