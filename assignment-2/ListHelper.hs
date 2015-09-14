module ListHelper where

-- Replace the head of a list with another element
replaceHead :: a -> [a] -> [a]
replaceHead _ []     = []
replaceHead y (_:ys) = y:ys

-- 'Rotate' the head a list.
-- rotate 4 "ABCDEFG" = "EFGABCD"
-- rotate (-2) "ABCDEFG" = "FGABCDE"
rotate :: Int -> [a] -> [a]
rotate x l =  (iterate f l) !! (x `mod` length l)
  where f []     = []
        f (x':xs) = xs ++ [x']

