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

  decomp :: Integer -> (Integer,Integer)
  decomp n = decomp' (0,n) where
    decomp' = until (odd.snd) (\ (m,n) -> (m+1,div n 2))

  primeMR :: Int -> Integer -> IO Bool
  primeMR _ 2 = return True
  primeMR 0 _ = return True
  primeMR k n = let 
     (r,s) = decomp (n-1) 
     f = \ x -> takeWhile (/= 1) 
         (map (\ j -> exM x (2^j*s) n)  [0..r])
    in 
     do 
      a <- randomRIO (1, n-1) :: IO Integer
      if exM a (n-1) n /= 1 
        then return False 
        else 
          if exM a s n /= 1 && last (f a) /= (n-1) 
            then return False
            else primeMR (k-1) n

  test_F :: Integer -> IO Integer
  test_F k = test_F_R (carmichael) k

  test_F_R :: [Integer] -> Integer -> IO Integer
  test_F_R [] _ = return 0
  test_F_R (h:t) k = do
      testResult <- prime_tests_F (fromIntegral k) h
      if not testResult then
        test_F_R t k
      else
        return h
        
  test_MR :: Int -> IO Integer
  test_MR k = do
    test_MR_R (carmichael) k 1000

  test_MR_R :: [Integer] -> Int -> Int -> IO Integer
  test_MR_R [] _ _ = return 0
  test_MR_R (h:t) k n = do
      testResult <- sequence $ take n $ repeat $ primeMR k h
      if any id testResult then
        return h
      else
        test_MR_R t k n


  carmichael :: [Integer]
  carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]