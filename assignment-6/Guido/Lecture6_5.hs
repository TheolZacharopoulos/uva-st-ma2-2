module Lecture6_5 where

import LeastFalsePositive
import Carmichael

main :: IO ()
main = do lfp <- mapM (least_false_positive_F 1 carmichael) [1..3]
          print lfp
