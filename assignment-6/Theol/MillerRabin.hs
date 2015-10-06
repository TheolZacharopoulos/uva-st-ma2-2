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