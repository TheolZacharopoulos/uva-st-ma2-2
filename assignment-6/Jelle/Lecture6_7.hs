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


  getLargeMersenne :: Integer -> Integer -> IO Integer
  getLargeMersenne p maxDepth = do
    if maxDepth == 0 then
      return p
    else do 
      m <- getNextMersenne p
    
      if m /= 0 then do
        result <- getLargeMersenne m (maxDepth - 1)
        return result
      else
        return p

  getNextMersenne :: Integer -> IO Integer
  getNextMersenne p = do
    isP <- primeMR (fromIntegral (2^p - 1)) 2
    if isP then
      return (2^p - 1)
    else
      return 0