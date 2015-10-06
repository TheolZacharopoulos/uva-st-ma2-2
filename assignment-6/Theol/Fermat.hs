module Fermat where 

import Lecture6Exm
import Composites

-- More functional style
test_fermat k = test_fermat' k composites
  where 
    test_fermat' k (c:cs) = do
    test_res <- prime_tests_F k c
    (case test_res of
          True  -> return c
          False -> (test_fermat' k cs))

runTest k = do
  xs <- sequence $ (take 100 $ repeat $ test_fermat k)
  return $ minimum $ xs

runTests = mapM runTest [1..3]