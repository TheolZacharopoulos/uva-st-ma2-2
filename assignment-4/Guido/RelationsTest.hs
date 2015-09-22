module RelationsTest where

import Test.QuickCheck
import Data.List
import Relations

isRel :: Rel Int -> Bool
isRel r = length r == length (nub r)

prop_hasOnlyInverse_inverseRel :: Rel Int -> Property
prop_hasOnlyInverse_inverseRel r = isRel r ==>
    isRel r'
    &&
    all (\(x,y) -> (y,x) `elem` r') r
    &&
    all (\(x,y) -> (y,x) `elem` r) r'
    where r' = inverseRel r

prop_hasOnlyBoth_unionRel :: Rel Int -> Rel Int -> Property
prop_hasOnlyBoth_unionRel r s = isRel r && isRel s ==>
    isRel u
    &&
    all (\e -> e `elem` r || e `elem` s) u
    &&
    null (r \\ u) && null (s \\ u)
    where u = unionRel r s

prop_hasOnlyOriginalAndInverse_symClos :: Rel Int -> Property
prop_hasOnlyOriginalAndInverse_symClos r = isRel r ==>
    isRel r'
    &&
    all (\e -> e `elem` r || e `elem` ri) r'
    &&
    null (r \\ r') && null (ri \\ r')
    where r' = symClos r
          ri = inverseRel r

prop_hasOnlyOriginalAndTransitive_trClos :: Rel Int -> Property
prop_hasOnlyOriginalAndTransitive_trClos r = isRel r ==>
    isRel r'
    &&
    all (\e -> e `elem` r || e `elem` rt) r'
    &&
    null (r \\ r') && null (rt \\ r')
    where r' = trClos r
          rt = r @@ r

