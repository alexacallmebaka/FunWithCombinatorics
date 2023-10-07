module Primitives where

import Control.Monad

-- # of permutations of n elements.
factorial :: Integer -> Maybe Integer
factorial 0 = Just 1
factorial n 
  | n < 0 = Nothing
  | otherwise = (n*) <$> factorial (n-1)

-- # of ways to choose a subset of k from n elements.
choose :: Integer -> Integer -> Integer
n `choose` 0 = 1
n `choose` k
  | n == k = 1
  | k > n = 0
  | otherwise = ( (n-1) `choose` (k-1) ) + ( (n-1) `choose` k ) 

-- # of permutations of n elements with repeated symbols. the xs define the number of times each symbol is repeated
-- ex. multinom 6 [2,3,1] is the # of permutations with 2, 3, and 1 duplicate elements of distinct types.
-- another way to think of this is orderings of the multiset {a,a,b,b,b,c}.
multinom :: Integer -> [Integer] -> Maybe Integer
multinom n xs
  | sum xs /= n = Nothing
  | otherwise = do
             num <- factorial n
             den <- foldl ( liftM2 (*) ) ( Just 1 ) $ map factorial xs
             quot <- return $ num `div` den
             return quot
