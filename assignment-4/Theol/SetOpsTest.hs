module SetOpsTest where

import SetOps
import Data.List
import SetOrd
import Test.QuickCheck
import QCSetGenerator

-----------------
-- Intersection

-- The intersection of an set with an empty set,
-- should be an empty set.
prop_intersectionSet_empty :: Set Int -> Bool
prop_intersectionSet_empty set = 
    (intersectSet set emptySet) == emptySet

-- The intersection of a set with itelf,
-- should be the set.
prop_intersectionSet_Idempotence :: Set Int -> Bool
prop_intersectionSet_Idempotence set = 
    (intersectSet set set) == set

prop_intersectionSet_Communitativity :: Set Int -> Set Int -> Bool
prop_intersectionSet_Communitativity s1 s2 =
    (intersectSet s1 s2) == (intersectSet s2 s1)

prop_intersectionSet_Associativity :: Set Int -> Set Int -> Set Int -> Bool
prop_intersectionSet_Associativity s1 s2 s3 =
    (intersectSet s1 (intersectSet s2 s3)) == (intersectSet (intersectSet s1 s2) s3)

prop_intersectionSet_Distributivity :: Set Int -> Set Int -> Set Int -> Bool
prop_intersectionSet_Distributivity s1 s2 s3 =
    (s1 `unionSet` (s2 `intersectSet` s3)) == (s1 `unionSet` s2) `intersectSet` (s1 `unionSet` s3) 

-----------------
-- Union

-- An element that exists in one of the two sets, 
-- it is also exists in their union.
prop_unionSet_elemIn :: Set Int -> Set Int -> Bool
prop_unionSet_elemIn s1@(Set xs) s2@(Set ys) = 
   all (==True) $ map (\x -> x `inSet` allElems || x `inSet` allElems) (xs ++ ys) 
        where
            allElems = unionSet s1 s2

-- The intersection of an set with an empty set,
-- should be the set itself.
prop_unionSet_empty :: Set Int -> Bool
prop_unionSet_empty set = 
    (unionSet set emptySet) == set

-- The union of a set with itelf,
-- should be the set.
prop_unionSet_Idempotence :: Set Int -> Bool
prop_unionSet_Idempotence set = 
    (unionSet set set) == set

prop_unionSet_Communitativity :: Set Int -> Set Int -> Bool
prop_unionSet_Communitativity s1 s2 =
    (unionSet s1 s2) == (unionSet s2 s1)

prop_unionSet_Associativity :: Set Int -> Set Int -> Set Int -> Bool
prop_unionSet_Associativity s1 s2 s3 =
    (unionSet s1 (unionSet s2 s3)) == (unionSet (unionSet s1 s2) s3)

prop_unionSet_Distributivity :: Set Int -> Set Int -> Set Int -> Bool
prop_unionSet_Distributivity s1 s2 s3 =
    (s1 `intersectSet` (s2 `unionSet` s3)) == (s1 `intersectSet` s2) `unionSet` (s1 `intersectSet` s3) 

----------------
-- Difference
-- TODO:

