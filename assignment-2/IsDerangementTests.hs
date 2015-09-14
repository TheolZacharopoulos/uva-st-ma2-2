module IsDerangementTests where

import Data.List
import System.Random

import IsDerangement
import Lecture2Test
import RandomHelper

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

-- This function generates two random lists with different lengths.
-- Case for 1 of the postconditions:
-- All elements in list a at some index i, are not in list b at index i
samePositionElementsCase :: IO ([Integer], [Integer])
samePositionElementsCase = do
    n <- randomRIO (1, limit)
    l <- getRandomList n limit
    return (l, l)

-- This function generates two random lists with elements occuring on the same index in both.
-- Case for 1 of the postconditions:
-- Lists of a different length are not derangements of each other.
differentLengthCase :: IO ([Integer], [Integer])
differentLengthCase = do
    length1 <- randomRIO (1, limit)
    length2 <- randomRIO (0, length1 - 1)
    l1 <- getRandomList length1 limit
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
    l1 <- getRandomList length1 limit
    x <- randomRIO (1, length1 - 1)
    let nElements = if length1 == 1 then 1 else x
    shuffledL1 <- shuffle l1
    l2 <- getRandomListWithExclusions 
            length1 
            limit
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

-- Execute all the tests.
testAllDerangement :: IO ()
testAllDerangement = sequence_ allDerangementTests
