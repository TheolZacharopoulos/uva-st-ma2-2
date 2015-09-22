{-# LANGUAGE TemplateHaskell #-}
module RelationsTest where

{- We use QuickCheck to test some invariants of the relations. 
 - See below for which invariants are tested.
 -
 - Because `inverseRel` and `unionRel` are custom functions and are used
 - in `symClos` and `trClos` these are also tested.
 -}

import Test.QuickCheck
import Test.QuickCheck.All
import Data.List
import Relations

newtype TestRel a = TestRel (Rel a)
    deriving (Show)

instance (Arbitrary a, Ord a) => Arbitrary (TestRel a) where
    arbitrary = do
        values <- (suchThat orderedList isUniqueList)
        return $ TestRel values
        where
            isUniqueList xs = (nub xs == xs)

isRel :: Rel Int -> Bool
isRel r = length r == length (nub r) && sort r == r

-- (inverseRel r) is a valid relation
prop_isRel_inverseRel :: TestRel Int -> Bool
prop_isRel_inverseRel (TestRel r) = isRel (inverseRel r)

-- (inverseRel r) contains all the inverted pairs of r.
prop_hasAll_inverseRel :: TestRel Int -> Bool
prop_hasAll_inverseRel (TestRel r) = 
    all (\(x,y) -> (y,x) `elem` r') r
    where r' = inverseRel r

-- r contains all the inverted pairs of (inverseRel r).
prop_hasOnly_inverseRel :: TestRel Int -> Bool
prop_hasOnly_inverseRel (TestRel r) = 
    all (\(x,y) -> (y,x) `elem` r) (inverseRel r)

-- inverseRel is an `involution` 
prop_involution_inverseRel :: TestRel Int -> Bool
prop_involution_inverseRel (TestRel r) =
    (inverseRel.inverseRel) r == r

-- (unionRel r) is a valid relation
prop_isRel_unionRel :: TestRel Int -> TestRel Int -> Bool
prop_isRel_unionRel (TestRel r) (TestRel s) = isRel (unionRel r s)

-- (unionRel r s) contains all the elements of r and s.
prop_hasAll_unionRel :: TestRel Int -> TestRel Int -> Bool
prop_hasAll_unionRel (TestRel r) (TestRel s) =
    all (flip elem u) (r ++ s)
    where u = unionRel r s

-- (unionRel r s) contains only elements in either r or s.
prop_hasOnly_unionRel :: TestRel Int -> TestRel Int -> Bool
prop_hasOnly_unionRel (TestRel r) (TestRel s) = 
    all (\e -> e `elem` r || e `elem` s) (unionRel r s)

-- Invariants on (symClos r)
-- 1. is a valid relation
-- 2. contains all the elements that are in either r or (inverseRel r)
-- 3. contains only the elements that are in either r or (inverseRel r)
prop_hasOnlyOriginalAndInverse_symClos :: TestRel Int -> Bool
prop_hasOnlyOriginalAndInverse_symClos (TestRel r) =
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
prop_hasOnlyOriginalAndTransitive_trClos :: TestRel Int -> Bool
prop_hasOnlyOriginalAndTransitive_trClos (TestRel r) =
    isRel r'
    &&
    all (\e -> e `elem` r || e `elem` rt) r'
    &&
    null (r \\ r') && null (rt \\ r')
    where r' = trClos r
          rt = r @@ r

return []
runTests = $quickCheckAll
