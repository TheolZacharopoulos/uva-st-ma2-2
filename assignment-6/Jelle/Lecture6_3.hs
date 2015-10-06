  module Lecture6
  
  where 
  
  import Data.List
  import System.Random

  factors_naive :: Integer -> [Integer]
  factors_naive n = factors' n 2 where 
    factors' 1 _ = []
    factors' n m 
      | n `mod` m == 0 = m : factors' (n `div` m) m
      | otherwise      =     factors' n (m+1)

  factors :: Integer -> [Integer]
  factors n = let 
     ps = takeWhile (\m -> m^2 <= n) primes
   in factors' n ps where 
     factors' 1 _  = []
     factors' n [] = [n]
     factors' n (p:ps) 
      | n `mod` p == 0 = p: factors' (n `div` p) (p:ps)
      | otherwise      =    factors' n ps

  isPrime n = factors n == [n]
  primes = 2 : filter isPrime [3..]

  composites :: [Integer]
  composites = 1 : filter (not . isPrime) [2..]
