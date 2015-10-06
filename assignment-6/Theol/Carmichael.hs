module Carmichael where 

import Lecture6Exm
import Composites

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

test_carmichael k = test_fermat' k carmichael
  where 
    test_fermat' k (c:cs) = do
      test_res <- prime_tests_F k c
      (case test_res of
            True  -> return c
            False -> (test_fermat' k cs))

runTestC k = do
  xs <- sequence $ (take 100 $ repeat $ test_carmichael k)
  return $ minimum $ xs

runTestsC = mapM runTestC [1..3]