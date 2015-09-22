module SetOperators where

import SetOrd
import Data.List


-- Already specifed in SetOrd.hs
--unionSet :: (Ord a) => Set a -> Set a -> Set a 
--unionSet (Set [])     set2  =  set2
--unionSet (Set (x:xs)) set2  = 
--   insertSet x (unionSet (Set xs) set2)


intersectionSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set []) set2 = Set []
intersectionSet (Set (x:xs)) set2 =
    if x `inSet` set2 then
        insertSet x (intersectionSet (Set xs) set2)
    else
        intersectionSet (Set xs) set2

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set []) set2 = set2
differenceSet (Set (x:xs)) set2 = 
    if x `inSet` set2 then
        differenceSet (Set xs) (deleteSet x set2)
    else
        insertSet x (differenceSet (Set xs) set2)
    