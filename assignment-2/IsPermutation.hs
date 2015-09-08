module IsPermutation where

import Data.List

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys
                      &&
                      length (intersect xs ys) == length xs