module SetOps where

import Data.List
import SetOrd

-- card :: Set a -> Integer
-- card (Set xs) = length xs

intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet (Set xs) (Set ys) = Set $ intersect xs ys
-- intersectSet small@(Set xs) large@(Set ys)
--    | card small > card large = intersectSet' ys xs
--    | otherwise               = intersectSet' xs ys 
--  where intersectSet' xs' ys' = Set $ foldr insert' emptySet xs'
--            where insert' x s@(Set zs) = if inSet x ys'
--                                         then insertSet x s
--                                         else s

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set xs) (Set ys) = Set $ xs \\ ys
