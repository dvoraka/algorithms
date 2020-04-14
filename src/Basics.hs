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
