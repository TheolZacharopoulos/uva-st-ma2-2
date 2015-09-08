module PermutationsCheck where

import Data.List

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation l1 l2 = length l1 == length l2 && length (intersect l1 l2) == length l1