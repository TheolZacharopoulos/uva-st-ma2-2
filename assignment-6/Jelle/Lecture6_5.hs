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

  modularExp :: Integer -> Integer -> Integer -> Integer
  modularExp _ 0 m = 1 `mod` m
  modularExp b e m =  
    if odd e then
       (result * b) `mod` m
    else
      result
    where
      result = (modularExp ((b * b) `mod` m) (e `div` 2) m)

  exM :: Integer -> Integer -> Integer -> Integer
  exM = modularExp

  prime_test_F :: Integer -> IO Bool
  prime_test_F n = do 
     a <- randomRIO (1, n-1) :: IO Integer
     return (exM a (n-1) n == 1)

  prime_tests_F :: Int -> Integer -> IO Bool
  prime_tests_F k n = do
   as <- sequence $ fmap (\_-> randomRIO (1,n-1)) [1..k]
   return (all (\ a -> exM a (n-1) n == 1) as)
 
  test_F :: Int -> IO Integer
  test_F k = do
    test_F_R (carmichael) k 1000

  test_F_R :: [Integer] -> Int -> Int -> IO Integer
  test_F_R [] _ _ = return 0
  test_F_R (h:t) k n = do
      testResult <- sequence $ take n $ repeat $ prime_tests_F k h
      if any id testResult then
        return h
      else
        test_F_R t k n

  carmichael :: [Integer]
  carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]