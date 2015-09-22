module RelationsTest where

{- We use QuickCheck to test some invariants of the relations. 
 - See below for which invariants are tested.
 -
 - Because `inverseRel` and `unionRel` are custom functions and are used
 - in `symClos` and `trClos` these are also tested.
 -}

import Test.QuickCheck
import Data.List
import Relations

isRel :: Rel Int -> Bool
isRel r = length r == length (nub r)

-- Invariants on (inverseRel r)
-- 1. is a valid relation
-- 2. contains all the inverted pairs of r.
-- 3. contains only the inverted pairs of r.
prop_hasOnlyInverse_inverseRel :: Rel Int -> Property
prop_hasOnlyInverse_inverseRel r = isRel r ==>
    isRel r'
    &&
    all (\(x,y) -> (y,x) `elem` r') r
    &&
    all (\(x,y) -> (y,x) `elem` r) r'
    where r' = inverseRel r

-- Invariants on (unionRel r s)
-- 1. is a valid relation
-- 2. contains all the elements that are in either r or s
-- 3. contains only the elements that are in either r or s
prop_hasOnlyBoth_unionRel :: Rel Int -> Rel Int -> Property
prop_hasOnlyBoth_unionRel r s = isRel r && isRel s ==>
    isRel u
    &&
    all (\e -> e `elem` r || e `elem` s) u
    &&
    null (r \\ u) && null (s \\ u)
    where u = unionRel r s

-- Invariants on (symClos r)
-- 1. is a valid relation
-- 2. contains all the elements that are in either r or (inverseRel r)
-- 3. contains only the elements that are in either r or (inverseRel r)
prop_hasOnlyOriginalAndInverse_symClos :: Rel Int -> Property
prop_hasOnlyOriginalAndInverse_symClos r = isRel r ==>
    isRel r'
    &&
    all (\e -> e `elem` r || e `elem` ri) r'
    &&
    null (r \\ r') && null (ri \\ r')
    where r' = symClos r
          ri = inverseRel r

-- Invariants on (trClos r)
-- 1. is a valid relation
-- 2. contains all the elements that are in either r or (r @@ r)
-- 3. contains only the elements that are in either r or (r @@ r)
prop_hasOnlyOriginalAndTransitive_trClos :: Rel Int -> Property
prop_hasOnlyOriginalAndTransitive_trClos r = isRel r ==>
    isRel r'
    &&
    all (\e -> e `elem` r || e `elem` rt) r'
    &&
    null (r \\ r') && null (rt \\ r')
    where r' = trClos r
          rt = r @@ r
