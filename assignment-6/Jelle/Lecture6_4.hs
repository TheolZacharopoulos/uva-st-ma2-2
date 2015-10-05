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
  composites = 0 : filter (not . isPrime) [1..]

  expM ::  Integer -> Integer -> Integer -> Integer
  expM x y = rem (x^y)

  exM :: Integer -> Integer -> Integer -> Integer
  exM = expM -- to be replaced by a fast version

  prime_test_F :: Integer -> IO Bool
  prime_test_F n = do 
     a <- randomRIO (1, n-1) :: IO Integer
     return (exM a (n-1) n == 1)

  prime_tests_F :: Int -> Integer -> IO Bool
  prime_tests_F k n = do
   as <- sequence $ fmap (\_-> randomRIO (1,n-1)) [1..k]
   return (all (\ a -> exM a (n-1) n == 1) as)

  test_F :: Integer -> IO Integer
  test_F k = test_F_R (tail composites) k

  test_F_R :: [Integer] -> Integer -> IO Integer
  test_F_R [] _ = return 0
  test_F_R (h:t) k = do
      testResult <- prime_tests_F (fromIntegral k) h
      if not testResult then
        test_F_R t k
      else
        return h