-- |
-- Module      :  Basics
-- License     :  see LICENSE
--
-- Basic algorithms.
--
module Basics where

-- | Creates list of pairs of adjacent elements from the given list.
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- | Returns true if the list is sorted.
sorted :: Ord a => [a] -> Bool
sorted xs = and [u <= v | (u, v) <- pairs xs]

-- | The Euclidean algorithm calculates the greatest common divisor(GCD) of two natural numbers a and b.
euclid :: Integral n => n -> n -> n
euclid a b =
  if remainder == 0
    then b
    else euclid b remainder
  where
    remainder = a `mod` b

-- | Inserts data in a correct order into the sorted list.
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

-- | Insertion sort.
isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

-- | Quick sort.
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [s | s <- xs, s <= x]
    larger = [l | l <- xs, l > x]
