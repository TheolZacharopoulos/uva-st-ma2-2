module QCSetGenerator where 

import SetOrd
import Test.QuickCheck
import Control.Monad (replicateM)
import Data.List

-- Generic arbitrary Set generator.
-- Without using list2set
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = do 
            values <- (suchThat orderedList isUniqueList)
            return $ Set(values)
            where isUniqueList xs = (nub xs == xs) 

-- Generic arbitrary Set generator.
-- Using list2set
--instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
--    arbitrary = 
--        sized $ \size -> do 
--            len <- choose (0, size)
--            values <- replicateM len arbitrary 
--            return $ list2set values

-- Generate random Int Sets
genIntSetQC = sample (arbitrary :: Gen (Set Int))
