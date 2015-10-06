module Fermat where 

import Lecture6Exm
import Composites

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