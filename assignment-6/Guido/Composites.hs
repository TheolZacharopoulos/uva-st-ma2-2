module Composites where

import Lecture6

composites :: [Integer]
composites = foldr f [] (iterate (+2) 4)
    where f nEven knownComposites = let nOdd = nEven + 1
                                    in  if isPrime nOdd
                                        then nEven : knownComposites
                                        else nEven : nOdd : knownComposites
