module MillerRabin where 

import Lecture6Exm
import Composites
import Carmichael

test_carmichael_mr k = primeMR' k carmichael
  where 
    primeMR' k (c:cs) = do
      test_res <- primeMR k c
      (case test_res of
            True  -> return c
            False -> (primeMR' k cs))

runTestMR k = do
  xs <- sequence $ (take 100 $ repeat $ test_carmichael_mr k)
  return $ minimum $ xs

runTestsMr = mapM runTestMR [1..3]