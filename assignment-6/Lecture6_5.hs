module Lecture6_5 where

import Lecture6
import LeastFalsePositive
import Carmichael

main :: IO ()
main = do lfp <- mapM flfp [1..3]
          print lfp
    where flfp = least_false_positive prime_tests_F (\_ -> 10000) carmichael
