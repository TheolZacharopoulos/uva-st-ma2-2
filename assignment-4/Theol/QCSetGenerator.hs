module QCSetGenerator where 

import SetOrd
import Test.QuickCheck
import Control.Monad (replicateM)

-- Generic arbitrary Set generator.
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = 
        sized $ \size -> do 
            len <- choose (0, size)
            values <- replicateM len arbitrary 
            return $ list2set values
