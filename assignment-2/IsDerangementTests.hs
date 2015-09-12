module DerangementTests where

import IsDerangement
import Lecture2Test

import Data.List
import System.Random
import Data.Array.IO
import Control.Monad

-- Preconditions:
-- Both lists are of the same type
-- An Eq function exists for elements of the lists
-- The lists contain no duplicate elements

-- Postconditions:
-- Returns true iff:
--     The lists contain the same elements
--     The lists have the same length
--     All elements in list a at some index i, are not in list b at index i
-- If returns true for input a, b then returns true for b, a

limit :: Integer
limit = 10

-- Helper function
uncurriedIsDerangement :: Eq a => ([a], [a]) -> Bool
uncurriedIsDerangement (xs, ys) = isDerangement xs ys

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
-- Generates a random Integer list.
getRandomList :: Integer -> IO [Integer]
getRandomList 0 = do
    return []
getRandomList x = do
    r <- randomRIO (-limit, limit)
    l <- getRandomList (x - 1)
    return $ r : l

-- Helper function
-- Generates a random Integer between `lower` and `upper` limit,
-- excluding a list of integers `exclusions`
randomRangeWithExclusions :: Integer -> Integer -> [Integer]
                          -> IO Integer
randomRangeWithExclusions lower upper exclusions = do
    x <- randomRIO (lower, upper) 
    if length (intersect [x] exclusions) == 1
      then randomRangeWithExclusions lower upper exclusions
      else return x

-- Helper function
-- Generates a random list of n Integers excluding a list of integers `exclusions`
getRandomListWithExclusions :: Integer -> [Integer] -> IO [Integer]
getRandomListWithExclusions 0 _ = return []
getRandomListWithExclusions x exclusions = do
    r <- randomRangeWithExclusions (-limit) limit exclusions
    l <- getRandomListWithExclusions (x - 1) exclusions
    return $ r : l

-- This function generates two random lists with different lengths.
-- Case for 1 of the postconditions:
-- All elements in list a at some index i, are not in list b at index i
samePositionElementsCase :: IO ([Integer], [Integer])
samePositionElementsCase = do
    n <- randomRIO (1, limit)
    l <- getRandomList n
    return (l, l)

-- This function generates two random lists with elements occuring on the same index in both.
-- Case for 1 of the postconditions:
-- Lists of a different length are not derangements of each other.
differentLengthCase :: IO ([Integer], [Integer])
differentLengthCase = do
    length1 <- randomRIO (1, limit)
    length2 <- randomRIO (0, length1 - 1)
    l1 <- getRandomList length1
    shuffledL1 <- shuffle l1  
    return  (l1, take (fromIntegral length2) shuffledL1)

-- This function generates two lists of which one is a derangement of the other.
-- Positive case, matching all requirements for the two lists to be derangements.
-- Tests the following postconditions:
-- Lists containing the same elements, that are of the same length, and for
-- all elements in list a at some index i are not in list b at index i, are
-- derangements of each other.
derangementCase :: IO ([Integer], [Integer])
derangementCase = do
    lowerborder <- randomRIO (-limit, limit - 3)
    upperborder <- randomRIO (lowerborder + 3, limit)
    let l1 = [lowerborder .. upperborder] 
    takeNumber <- randomRIO (1, length l1 - 2)
    let l2 = (drop takeNumber l1) ++ (take takeNumber l1)
    return (l1, l2)

-- This function generates two lists which one has different elements than the other.
-- Case for 1 of the postconditions:
-- Lists containing different elements are not derangements of each other.
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
    testPost uncurriedIsDerangement (== False) differentLengthCase 

-- Test the derangment lists, case.
-- Input: two lists which one is the derangement of the other.
-- Expectation: 'True'
testDerangementCase :: IO ()
testDerangementCase = 
    testPost uncurriedIsDerangement (== True) derangementCase 

-- Test the different elements lists, case.
-- Input: two lists which one has (at least one) different element.
-- element(s) than the other.
-- Expectation: 'False'
testDifferentElementsCase :: IO ()
testDifferentElementsCase = 
    testPost uncurriedIsDerangement (== False) differentElementsCase

-- Test the same position elements list, case
-- Input: two lists which have elements on the same index in both.
-- Expectation: 'False'
testSamePositionElements :: IO()
testSamePositionElements =
    testPost uncurriedIsDerangement (== False) samePositionElementsCase

-- Test if the reverse is also a derangment:
-- i.e. if a is a derangement of b, then b is a derangement of a.
-- Expectation: 'True'
testSymmetricDerangementCase :: IO()
testSymmetricDerangementCase =
    testPost 
        (\(a, b) -> (isDerangement a b) && (isDerangement b a))
        (== True) 
        derangementCase

-- A list with all the tests.
allDerangementTests = [
    testDifferentLengthCase,
    testDerangementCase,
    testDifferentElementsCase,
    testSamePositionElements,
    testSymmetricDerangementCase
    ]

-- Execute all the triangle tests.
testAllDerangement :: IO [()]
testAllDerangement = sequence allDerangementTests