module SetIntGenerator where

import SetOrd
import System.Random
import Data.List

-- Generate a random sorted Set Int with length upto limit.
-- Length is uniformly chosen between 0 and limit.
getRandomSetInt :: Int -> IO (Set Int)
getRandomSetInt limit = do
    len <- randomRIO(0, limit)
    s <- getRandomSetIntR len emptySet
    return s 
    where       
        getRandomSetIntR :: Int -> (Set Int) -> IO (Set Int)
        getRandomSetIntR len s@(Set xs) = do
            if (length xs) == len then
                return s
            else do
                x <- randomRIO (0, len * 2)
                s2 <- getRandomSetIntR len (insertSet x s)
                return s2
