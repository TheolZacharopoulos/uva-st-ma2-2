module Lab4 where

import SetOrd
import Data.List
import System.Random
import Test.QuickCheck
import Control.Monad
import Data.Array.IO
import Data.Maybe

shuffleList :: [a] -> IO [a]
shuffleList xs = do
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

maxListLimit :: Int
maxListLimit = 1000

generateRandomList :: IO ([Int])
generateRandomList = do
    top <- randomRIO(1, maxListLimit)
    return [1..top]

generateRandomSet :: IO (Set Int)
generateRandomSet = do
    randomList <- generateRandomList
    shuffled <- shuffleList randomList
    return (Set shuffled)