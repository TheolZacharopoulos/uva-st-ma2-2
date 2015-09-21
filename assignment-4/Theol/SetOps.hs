module SetOps where

import Data.List
import SetOrd

card :: Set a -> Int
card (Set xs) = length xs

intersectSet :: (Ord a) => Set a -> Set a -> Set a
-- intersectSet (Set xs) (Set ys) = Set $ intersect xs ys
intersectSet small@(Set xs) large@(Set ys)
   | card small > card large = intersectSet' ys small
   | otherwise               = intersectSet' xs large 
   where intersectSet' xs' s = foldr insert' emptySet xs'
           where insert' x s' = if inSet x s
                                then insertSet x s'
                                else s'

differenceSet :: (Ord a) => Set a -> Set a -> Set a
-- differenceSet (Set xs) (Set ys) = Set $ xs \\ ys
differenceSet left@(Set xs) right = foldr insert' emptySet xs
   where insert' x s = if not (inSet x right)
                       then insertSet x s
                       else s
