module QCSetGenerator where 

import SetOrd
import Test.QuickCheck
import Control.Monad (replicateM)
import Data.List

-- Generic arbitrary Set generator.
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = do 
        values <- (suchThat orderedList isUniqueList)
        return $ Set(values)
        where 
            isUniqueList xs = (nub xs == xs) 

-- Generate random Int Sets
genIntSetQC = sample (arbitrary :: Gen (Set Int))
