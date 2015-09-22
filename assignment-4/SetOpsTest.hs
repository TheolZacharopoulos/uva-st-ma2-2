-- Set Operations Test properties
module SetOpsTest (main) where

import SetOps
import Data.List
import SetOrd
import Test.QuickCheck
import QCSetGenerator

-- Helper function, logical implication.
impl :: Bool -> Bool -> Bool
impl True False = False
impl _ _ = True

-----------------
-- Intersection

-- An element that exists in both sets, it exists in their intersection.
prop_intersectionSet_elemIn :: Set Int -> Set Int -> Bool
prop_intersectionSet_elemIn s1@(Set xs) s2@(Set ys) = 
    all (==True) $ map (\e -> (e `inSet` s1 && e `inSet` s2) `impl` (e `inSet` allElems)) (xs ++ ys)
    where
        allElems = intersectSet s1 s2

-- The intersection of a set with an empty set, is be an empty set.
prop_intersectionSet_empty :: Set Int -> Bool
prop_intersectionSet_empty set = (intersectSet set emptySet) == emptySet

-- The intersection of a set with itelf, is be the set.
prop_intersectionSet_Idempotence :: Set Int -> Bool
prop_intersectionSet_Idempotence set = (intersectSet set set) == set

prop_intersectionSet_Communitativity :: Set Int -> Set Int -> Bool
prop_intersectionSet_Communitativity s1 s2 = (intersectSet s1 s2) == (intersectSet s2 s1)

prop_intersectionSet_Associativity :: Set Int -> Set Int -> Set Int -> Bool
prop_intersectionSet_Associativity s1 s2 s3 =
    (intersectSet s1 (intersectSet s2 s3)) == (intersectSet (intersectSet s1 s2) s3)

prop_intersectionSet_Distributivity :: Set Int -> Set Int -> Set Int -> Bool
prop_intersectionSet_Distributivity s1 s2 s3 =
    (s1 `unionSet` (s2 `intersectSet` s3)) == (s1 `unionSet` s2) `intersectSet` (s1 `unionSet` s3) 

-----------------
-- Union

-- An element that exists in one of the two sets, it also exists in their union.
prop_unionSet_elemIn :: Set Int -> Set Int -> Bool
prop_unionSet_elemIn s1@(Set xs) s2@(Set ys) = 
   all (==True) $ map (\e -> e `inSet` allElems || e `inSet` allElems) (xs ++ ys) 
        where
            allElems = unionSet s1 s2

-- The intersection of a set with an empty set, is be the set itself.
prop_unionSet_empty :: Set Int -> Bool
prop_unionSet_empty set = (unionSet set emptySet) == set

-- The union of a set with itelf, is be the set.
prop_unionSet_Idempotence :: Set Int -> Bool
prop_unionSet_Idempotence set = (unionSet set set) == set

prop_unionSet_Communitativity :: Set Int -> Set Int -> Bool
prop_unionSet_Communitativity s1 s2 = (unionSet s1 s2) == (unionSet s2 s1)

prop_unionSet_Associativity :: Set Int -> Set Int -> Set Int -> Bool
prop_unionSet_Associativity s1 s2 s3 =
    (unionSet s1 (unionSet s2 s3)) == (unionSet (unionSet s1 s2) s3)

prop_unionSet_Distributivity :: Set Int -> Set Int -> Set Int -> Bool
prop_unionSet_Distributivity s1 s2 s3 =
    (s1 `intersectSet` (s2 `unionSet` s3)) == (s1 `intersectSet` s2) `unionSet` (s1 `intersectSet` s3) 

----------------
-- Difference

-- An element that exists in both sets, it does not exist in their difference.
prop_differenceSet_elemNotIn :: Set Int -> Set Int -> Bool
prop_differenceSet_elemNotIn s1@(Set xs) s2@(Set ys) = 
    all (==True) $ map (\e -> (e `inSet` s1 && (not(e `inSet` s2))) `impl` (e `inSet` allElems)) (xs ++ ys)
    where
        allElems = differenceSet s1 s2

-- The difference between a set and itself, is the empty set.
prop_differenceSet_self :: Set Int -> Bool
prop_differenceSet_self set = (differenceSet set set) == emptySet

-- The diference of a set with an empty set, is be the set itself.
prop_differenceSet_empty :: Set Int -> Bool
prop_differenceSet_empty set = (differenceSet set emptySet) == set

---------------------------------
-- Test runner options
options :: Args
options = Args {
    replay = Nothing,
    maxSuccess = 100,
    maxDiscardRatio = 100,
    maxSize = 100,
    chatty = True
}

runTests :: Args -> IO ()
runTests args = do
    f prop_intersectionSet_elemIn "prop_intersectionSet_elemIn OK?"
    f prop_intersectionSet_empty "prop_intersectionSet_empty OK?"
    f prop_intersectionSet_Idempotence "prop_intersectionSet_Idempotence OK?"
    f prop_intersectionSet_Communitativity "prop_intersectionSet_Communitativity OK?"
    f prop_intersectionSet_Distributivity "prop_intersectionSet_Distributivity OK?"

    f prop_unionSet_elemIn "prop_unionSet_elemIn OK?"
    f prop_unionSet_empty "prop_unionSet_empty OK?"
    f prop_unionSet_Idempotence "prop_unionSet_Idempotence OK?"
    f prop_unionSet_Associativity "prop_unionSet_Associativity OK?"
    f prop_unionSet_Distributivity "prop_unionSet_Distributivity OK?"

    f prop_differenceSet_elemNotIn "prop_differenceSet_elemNotIn OK?"
    f prop_differenceSet_self "prop_differenceSet_self OK?"
    f prop_differenceSet_empty "prop_differenceSet_empty OK?"

    where
        f prop str = do
            putStrLn str
            quickCheckWithResult args prop
            return ()

main :: IO ()
main = do
    runTests options