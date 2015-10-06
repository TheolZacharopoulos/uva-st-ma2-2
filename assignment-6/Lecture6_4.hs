module Lecture6_4 where

import Lecture6
import LeastFalsePositive
import Composites

main :: IO ()
main = do lfp <- mapM flfp [1..3]
          print lfp
    where flfp = least_false_positive prime_tests_F (\_ -> 1000) composites
