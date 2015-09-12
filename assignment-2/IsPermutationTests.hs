module PermutationTests where

import IsPermutation
import Lecture2Test

import Data.List
import System.Random
import Data.Array.IO
import Control.Monad

-- Preconditions:
-- Both lists are of the same type
-- An Eq function exists for elements of the lists 

-- Postconditions:
-- Returns true iff:
--     The lists contain the same elements
--     The lists have the same length
-- If returns true for input a, b then returns true for b, a

limit :: Integer
limit = 10

-- Helper function
-- Curies the isPermutation function.
curriedIsPermutation :: Eq a => ([a], [a]) -> Bool
curriedIsPermutation (l1, l2) = isPermutation l1 l2

-- Helper function
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

-- Helper function
-- Generates a random Integer between `lower` and `upper` limit,
-- excluding a list of integers `exclusions`
randomRangeWithExclusions :: Integer -> Integer -> [Integer] -> IO Integer
randomRangeWithExclusions lower upper exclusions = do
    x <- randomRIO (lower, upper) 
    if length (intersect [x] exclusions) == 1
      then randomRangeWithExclusions lower upper exclusions
      else return x

-- Helper function
-- Generates a random list of n Integers excluding a list of integers `exclusions`
getRandomListWithExclusions :: Integer -> [Integer] -> IO [Integer]
getRandomListWithExclusions 0 _ = return []
getRandomListWithExclusions n exclusions = do
    r <- randomRangeWithExclusions (-limit) limit exclusions
    l <- getRandomListWithExclusions (n - 1) exclusions
    return $ r : l

-- Helper function
-- Generates a random Integer list.
getRandomList :: Integer -> IO [Integer]
getRandomList 0 = do
    return []
getRandomList x = do
    r <- randomRIO (-limit, limit)
    l <- getRandomList (x - 1)
    return $ r : l

-- This function generates two random lists with different lengths.
-- Case for 1 of the postconditions:
-- Lists of a different length are not permutations of each other.
differentLengthCase :: IO ([Integer], [Integer])
differentLengthCase = do
    length1 <- randomRIO (1, limit)
    length2 <- randomRIO (0, length1 - 1)
    l1 <- getRandomList length1
    shuffledL1 <- shuffle l1  
    return  (l1, take (fromIntegral length2) shuffledL1)

-- This function generates two lists of which one is a permutation of the other.
-- Case for 1 of the postcondition:
-- Lists containing the same elements and of the same length are permutations of each other.
permutationCase :: IO ([Integer], [Integer])
permutationCase = do
    length1 <- randomRIO(1, limit)
    l1 <- getRandomList length1
    l2 <- shuffle l1
    return (l1, l2)

-- This function generates two lists which one has different elements than the other.
-- Case for 1 of the postconditions:
-- Lists containing different elements are not permutations of each other.
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

-- Test the different length lists, case.
-- Input: two lists with different lengths.
-- Expectation: 'False'
testDifferentLengthCase :: IO ()
testDifferentLengthCase = 
    testPost curriedIsPermutation (== False) differentLengthCase

-- Test the permutation lists, case.
-- Input: two lists which one is the permutation of the other.
-- Expectation: 'True'
testPermutationCase :: IO ()
testPermutationCase = 
    testPost curriedIsPermutation (== True) permutationCase 

-- Test the different elements lists, case.
-- Input: two lists which one has (at least one) different 
-- element(s) than the other.
-- Expectation: 'False'
testDifferentElementsCase :: IO ()
testDifferentElementsCase = 
    testPost curriedIsPermutation (== False) differentElementsCase


-- Test if the reverse is also a permutation:
-- i.e. if a is a permuation of b, then b is a permutation of a.
-- Expectation: 'True'
testReversePermutationCase :: IO()
testReversePermutationCase =
    testPost 
        (\(a, b) -> (isPermutation a b) && (isPermutation b a))
        (== True) 
        permutationCase