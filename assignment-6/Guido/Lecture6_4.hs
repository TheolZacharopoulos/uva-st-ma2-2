module Lecture6_4 where

import LeastFalsePositive
import Composites

main :: IO ()
main = do lfp <- mapM (least_false_positive_F 1000 composites) [1..3]
          print lfp
