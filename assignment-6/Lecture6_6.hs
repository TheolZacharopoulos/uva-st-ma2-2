module Lecture6_6 where

import Lecture6
import LeastFalsePositive
import Carmichael

main :: IO ()
main = do lfp <- mapM flfp [1..3]
          print lfp
    where flfp = least_false_positive primeMR (\_ -> 10000) carmichael
