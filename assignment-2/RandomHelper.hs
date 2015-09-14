module RandomHelper where

import Control.Monad
import Data.Array.IO
import Data.List
import Data.Maybe
import System.Random

-- Generates a random Integer between `lower` and `upper` limit, excluding a list of integers `exclusions`
randomRangeWithExclusions :: Integer -> Integer -> [Integer] -> IO (Maybe Integer)
randomRangeWithExclusions lower upper exclusions | upper < lower = return Nothing
                                                 | otherwise     = 
    if ((upper - lower) <= (toInteger $ length exclusions) &&
        ((sort exclusions) == [lower .. upper]))
    then return Nothing
    else do
        x <- randomRIO (lower, upper)
        if x `elem` exclusions
        then randomRangeWithExclusions lower upper exclusions
        else return (Just x)

-- Pick a random element from a list
randomElement :: [a] -> IO a
randomElement xs = do
    i <- randomRIO (0, length xs - 1)
    return $ xs !! i

-- Randomly shuffles a list.
-- source: https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

-- Generates a random list of n Integers excluding a list of integers `exclusions`
getRandomListWithExclusions :: Integer -> Integer -> [Integer] -> IO [Integer]
getRandomListWithExclusions 0 _ _ = return []
getRandomListWithExclusions n limit exclusions = do
    r <- randomRangeWithExclusions (-limit) limit exclusions
    if isNothing r
    then getRandomListWithExclusions n limit exclusions
    else do
        l <- getRandomListWithExclusions (n - 1) limit exclusions
        return $ (fromJust r) : l

-- Generates a random Integer list.
getRandomList :: Integer -> Integer -> IO [Integer]
getRandomList 0 _ = do
    return []
getRandomList x limit = do
    r <- randomRIO (-limit, limit)
    l <- getRandomList (x - 1) limit
    return $ r : l

