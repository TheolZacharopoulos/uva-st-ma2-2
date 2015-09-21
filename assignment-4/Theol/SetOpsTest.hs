module SetOpsTest where

import SetOps
import Data.List
import SetOrd
import Test.QuickCheck
import QCSetGenerator

-- An element that exists in one of the two sets, 
-- it is also exists in their union.
prop_unionSet_elemIn :: Set Int -> Set Int -> Bool
prop_unionSet_elemIn s1@(Set xs) s2@(Set ys) = 
   all (==True) $ map (\x -> x `inSet` allElems || x `inSet` allElems) (xs ++ ys) 
        where
            allElems = unionSet s1 s2
