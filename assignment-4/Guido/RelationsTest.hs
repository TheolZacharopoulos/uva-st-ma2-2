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

newtype TRel a = TRel (Rel a)
    deriving (Show)

instance (Arbitrary a, Ord a) => Arbitrary (TRel a) where
    arbitrary = do
        values <- (suchThat orderedList isUniqueList)
        return $ TRel values
        where
            isUniqueList xs = (nub xs == xs)

isRel :: Rel Int -> Bool
isRel r = length r == length (nub r) && sort r == r

-- (inverseRel r) is a valid relation
prop_isRel_inverseRel :: TRel Int -> Bool
prop_isRel_inverseRel (TRel r) = isRel (inverseRel r)

-- (inverseRel r) contains all the inverted pairs of r.
prop_hasAll_inverseRel :: Rel Int -> Property
prop_hasAll_inverseRel r = isRel r ==>
    all (\(x,y) -> (y,x) `elem` r') r
    where r' = inverseRel r

-- r contains all the inverted pairs of (inverseRel r).
prop_hasOnly_inverseRel :: Rel Int -> Property
prop_hasOnly_inverseRel r = isRel r ==>
    all (\(x,y) -> (y,x) `elem` r) (inverseRel r)

-- inverseRel is an `involution` 
prop_involution_inverseRel :: Rel Int -> Property
prop_involution_inverseRel r = isRel r ==>
    (inverseRel.inverseRel) r == r

-- (unionRel r) is a valid relation
prop_isRel_unionRel :: TRel Int -> TRel Int -> Bool
prop_isRel_unionRel (TRel r) (TRel s) = isRel (unionRel r s)

-- (unionRel r s) contains all the elements of r and s.
prop_hasAll_unionRel :: Rel Int -> Rel Int -> Property
prop_hasAll_unionRel r s = isRel r && isRel s ==>
    all (flip elem u) (r ++ s)
    where u = unionRel r s

-- (unionRel r s) contains only elements in either r or s.
prop_hasOnly_unionRel :: Rel Int -> Rel Int -> Property
prop_hasOnly_unionRel r s = isRel r && isRel s ==>
    all (\e -> e `elem` r || e `elem` s) (unionRel r s)

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

runTests = do
    sequence_ $ map (quickCheckWith args)
        [prop_isRel_inverseRel
        ]
    sequence_ $ map (quickCheckWith args)
        [prop_isRel_unionRel
        ]
    where args = Args {
        replay = Nothing,
        maxSuccess = 100,
        maxDiscardRatio = 100,
        maxSize = 100,
        chatty = True
    }
