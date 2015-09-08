module PermutationTests where

import IsPermutation
import Lecture2Test

import Data.List
import System.Random
import Data.Array.IO
import Control.Monad

curriedIsPermutation :: Eq a => ([a], [a]) -> Bool
curriedIsPermutation (l1, l2) = isPermutation l1 l2

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

getRandomList :: Integer -> IO [Integer]
getRandomList 0 = do
    return []
getRandomList x = do
    r <- randomRIO (-limit, limit)
    l <- getRandomList (x - 1)
    return $ r : l

differentLengthCase :: IO ([Integer], [Integer])
differentLengthCase = do
    length1 <- randomRIO (1, limit)
    length2 <- randomRIO (0, length1 - 1)
    l1 <- getRandomList length1
    shuffledL1 <- shuffle l1  
    return  (l1, take (fromIntegral length2) shuffledL1)

permutationCase :: IO ([Integer], [Integer])
permutationCase = do
    length1 <- randomRIO(1, limit)
    l1 <- getRandomList length1
    l2 <- shuffle l1
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
    testPost curriedIsPermutation (== False) differentLengthCase 

testPermutationCase :: IO ()
testPermutationCase = 
    testPost curriedIsPermutation (== True) permutationCase 

testDifferentElementsCase :: IO ()
testDifferentElementsCase = 
    testPost curriedIsPermutation (== False) differentElementsCase
