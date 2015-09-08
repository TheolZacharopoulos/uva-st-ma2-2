module IsPermutation where

import Data.List

isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation xs ys = (sort xs) == (sort ys)
