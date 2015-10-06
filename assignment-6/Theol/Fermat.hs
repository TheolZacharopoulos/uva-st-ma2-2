module Fermat

where 

import Data.List
import System.Random
import Control.Monad

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
composites = filter (not . isPrime) [2..]

exM :: Integer -> Integer -> Integer -> Integer
exM base 0 modulus = 1
exM base expo modulus = if (expo `mod` 2) == 1 then (result*base) `mod` modulus else result
  where result = exM ((base*base) `mod` modulus) (expo `div`2) modulus

prime_test_F :: Integer -> IO Bool
prime_test_F n = do 
  a <- randomRIO (1, n-1) :: IO Integer
  return (exM a (n-1) n == 1)

prime_tests_F :: Int -> Integer -> IO Bool
prime_tests_F k n = do
  as <- sequence $ fmap (\_-> randomRIO (1,n-1)) [1..k]
  return (all (\ a -> exM a (n-1) n == 1) as)

-- A bit imperative style
test_fermat k = do
  composite_test <- (mapM (prime_tests_F k) comps)
  let zipped = zip comps composite_test
  let trueOccurences = filter (\x -> snd x == True) zipped
  let firstTrue = fst (trueOccurences !! 1)
  return firstTrue
    where comps = (take 200000 composites)

-- More functional style
test_fermat_2 k = test_fermat_2' k composites
  where 
    test_fermat_2' k (c:cs) = do
    test_res <- prime_tests_F k c
    (case test_res of
          True  -> return c
          False -> (test_fermat_2' k cs))