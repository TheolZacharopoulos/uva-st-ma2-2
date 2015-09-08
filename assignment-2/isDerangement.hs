module IsDerangement where

import Data.List

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys && length (intersect xs ys) == length xs

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && all id (zipWith (/=) xs ys)

deran :: Eq a => [a] -> [[a]]
deran xs = filter (isDerangement xs) $ permutations xs