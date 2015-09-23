{-# LANGUAGE TemplateHaskell #-}
module RelationsTest where

{- We use QuickCheck to test some invariants of the relations. 
 - See below for which invariants are tested.
 -
 - Because `inverseRel` and `unionRel` are custom functions and are used
 - in `symClos` and `trClos` these are also tested.
 -}

import Data.List
import Data.Maybe
import Relations
import Test.QuickCheck
import Test.QuickCheck.All

-- In order to use QuickCheck we need to wrap the `Rel a` type in a `newtype`
-- datatype, otherwise we cannot define a sufficiently effective Arbitrary
-- generator (because `Rel a` is a type synonym).
newtype TestRel a = TestRel (Rel a)
    deriving (Show)

instance (Arbitrary a, Ord a) => Arbitrary (TestRel a) where
    arbitrary = do
        values <- (suchThat orderedList isUniqueList)
        return $ TestRel values
        where
            isUniqueList xs = (nub xs == xs)

-- Some helper functions to assert invariants on relations
isRel :: Rel Int -> Bool
isRel r = (sort.nub) r == r

involutory :: (Rel Int -> Rel Int) -> TestRel Int -> Bool
involutory f (TestRel r) = (f.f) r == r

idempotent :: (Rel Int -> Rel Int) -> TestRel Int -> Bool
idempotent f (TestRel r) = f r == f (f r)

commutative :: (Rel Int -> Rel Int -> Rel Int)
            -> TestRel Int -> TestRel Int -> Bool
commutative f (TestRel r) (TestRel s) = f r s == f s r

associative :: (Rel Int -> Rel Int -> Rel Int)
            -> TestRel Int -> TestRel Int -> TestRel Int -> Bool
associative f (TestRel r) (TestRel s) (TestRel t) = f (f r s) t == f r (f s t)

-- This succeeds if all relations in the `closure` argument can be
-- explained by zero or more transitive relations in the `base` argument.
explainTransitiveRelation :: Rel Int -> Rel Int -> (Int,Int) -> Bool
explainTransitiveRelation base closure r@(a,c) =
    r `elem` base ||
    (isJust maybeB &&
     explainTransitiveRelation base closure (a,b) &&
     explainTransitiveRelation base closure (b,c))
    where allAs         = filter ((== a).fst) closure
          allCs         = filter ((== c).snd) closure
          allPossibleBs = filter (\b -> b /= a && b /= c) $ map snd allAs ++ map fst allCs
          allBs         = allPossibleBs \\ nub allPossibleBs
          maybeB        = if null allBs then Nothing else Just (head allBs)
          b             = fromJust maybeB

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

-- inverseRel is 'involutory'
prop_involution_inverseRel = involutory inverseRel

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

-- unionRel is idempotent
prop_idempotence_unionRel (TestRel r) = idempotent (unionRel r)

-- unionRel is commutative
prop_commutativity_unionRel = commutative unionRel

-- unionRel is commutative
prop_associativity_unionRel = associative unionRel

-- symClos is a valid relation
prop_isRel_symClos :: TestRel Int -> Bool
prop_isRel_symClos (TestRel r) = isRel (symClos r)

-- (symClos r) contains all the elements from r and (inverseRel r)
prop_hasAll_symClos :: TestRel Int -> Bool
prop_hasAll_symClos (TestRel r) = 
    all (flip elem r') (r ++ inverseRel r)
    where r' = symClos r

-- (symClos r) contains only the elements in either r or (inverseRel r)
prop_hasOnly_symClos :: TestRel Int -> Bool
prop_hasOnly_symClos (TestRel r) =
    all (\e -> e `elem` r || e `elem` r') (symClos r)
    where r' = inverseRel r

-- symClos is idempotent
prop_idempotence_symClos = idempotent symClos

-- trClos is a valid relation
prop_isRel_trClos :: TestRel Int -> Bool
prop_isRel_trClos (TestRel r) = isRel (trClos r)

-- (trClos r) contains all elements r
prop_hasOriginal_trClos :: TestRel Int -> Bool
prop_hasOriginal_trClos (TestRel r) =
    all (flip elem r') r
    where r' = trClos r

-- (trClos r) contains all elements r @@ r
prop_hasTransitive_trClos :: TestRel Int -> Bool
prop_hasTransitive_trClos (TestRel r) =
    all (flip elem r') (r @@ r)
    where r' = trClos r

-- trClos is idempotent
prop_idempotence_trClos = idempotent trClos

-- explain every relation in (trClos r) by leading it back to relations in r.
-- This is necessary otherwise it's possible to create a nasty idempotent
-- and transitive (trClos r) that always returns a relation with a certain
-- element X, even though X is never in the original or transitive of r
prop_transitivity_trClos :: TestRel Int -> Bool
prop_transitivity_trClos (TestRel r) = all (explainTransitiveRelation r r') r
    where r' = trClos r

return []
runTests = $quickCheckAll
