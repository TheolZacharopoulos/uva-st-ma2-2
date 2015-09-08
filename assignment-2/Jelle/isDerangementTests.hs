module DerangementTests where

import IsDerangement
import Lecture2Test

import Data.List
import System.Random
import Data.Array.IO
import Control.Monad


limit :: Integer
limit = 10

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

getRandomList :: Integer -> IO [Integer]
getRandomList 0 = do
    return []
getRandomList x = do
    r <- randomRIO (-limit, limit)
    l <- getRandomList (x - 1)
    return $ r : l

randomRangeWithExclusions :: Integer -> Integer -> [Integer]
                          -> IO Integer
randomRangeWithExclusions lower upper exclusions = do
    x <- randomRIO (lower, upper) 
    if length (intersect [x] exclusions) == 1
      then randomRangeWithExclusions lower upper exclusions
      else return x

getRandomListWithExclusions :: Integer -> [Integer] -> IO [Integer]
getRandomListWithExclusions 0 _ = return []
getRandomListWithExclusions x exclusions = do
    r <- randomRangeWithExclusions (-limit) limit exclusions
    l <- getRandomListWithExclusions (x - 1) exclusions
    return $ r : l

curriedIsDerangement :: Eq a => ([a], [a]) -> Bool
curriedIsDerangement (xs, ys) = isDerangement xs ys


differentLengthCase :: IO ([Integer], [Integer])
differentLengthCase = do
    length1 <- randomRIO (1, limit)
    length2 <- randomRIO (0, length1 - 1)
    l1 <- getRandomList length1
    shuffledL1 <- shuffle l1  
    return  (l1, take (fromIntegral length2) shuffledL1)

derangementCase :: IO ([Integer], [Integer])
derangementCase = do
    lowerborder <- randomRIO (-limit, limit - 3)
    upperborder <- randomRIO (lowerborder + 3, limit)
    let l1 = [lowerborder .. upperborder] 
    takeNumber <- randomRIO (1, length l1 - 2)
    let l2 = (drop takeNumber l1) ++ (take takeNumber l1)
    return (l1, l2)

differentElementsCase :: IO ([Integer], [Integer])
differentElementsCase = do
    length1 <- randomRIO (1, limit)
    l1 <- getRandomList length1
    x <- randomRIO (1, length1 - 1)
    let nElements = if length1 == 1 then 1 else x
    shuffledL1 <- shuffle l1
    l2 <- getRandomListWithExclusions 
            length1 
            (take (fromIntegral nElements) shuffledL1)
    return (l1, l2)


testDifferentLengthCase :: IO ()
testDifferentLengthCase = 
    testPost curriedIsDerangement (== False) differentLengthCase 

testDerangementCase :: IO ()
testDerangementCase = 
    testPost curriedIsDerangement (== True) derangementCase 

testDifferentElementsCase :: IO ()
testDifferentElementsCase = 
    testPost curriedIsDerangement (== False) differentElementsCase