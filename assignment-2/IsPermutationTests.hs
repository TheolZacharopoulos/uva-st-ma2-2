module IsPermutationTests where

import Data.List
import System.Random

import IsPermutation
import Lecture2Test
import RandomHelper

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
-- Uncuries the isPermutation function.
unCurriedIsPermutation :: Eq a => ([a], [a]) -> Bool
unCurriedIsPermutation (l1, l2) = isPermutation l1 l2

-- This function generates two random lists with different lengths.
-- Case for 1 of the postconditions:
-- Lists of a different length are not permutations of each other.
differentLengthCase :: IO ([Integer], [Integer])
differentLengthCase = do
    length1 <- randomRIO (1, limit)
    length2 <- randomRIO (0, length1 - 1)
    l1 <- getRandomList length1 limit
    shuffledL1 <- shuffle l1  
    return  (l1, take (fromIntegral length2) shuffledL1)

-- This function generates two lists of which one is a permutation of the other.
-- Case for 1 of the postcondition:
-- Lists containing the same elements and of the same length are permutations of each other.
permutationCase :: IO ([Integer], [Integer])
permutationCase = do
    length1 <- randomRIO(1, limit)
    l1 <- getRandomList length1 limit
    l2 <- shuffle l1
    return (l1, l2)

-- This function generates two lists which one has different elements than the other.
-- Case for 1 of the postconditions:
-- Lists containing different elements are not permutations of each other.
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
    testPost unCurriedIsPermutation (== False) differentLengthCase

-- Test the permutation lists, case.
-- Input: two lists which one is the permutation of the other.
-- Expectation: 'True'
testPermutationCase :: IO ()
testPermutationCase = 
    testPost unCurriedIsPermutation (== True) permutationCase 

-- Test the different elements lists, case.
-- Input: two lists which one has (at least one) different 
-- element(s) than the other.
-- Expectation: 'False'
testDifferentElementsCase :: IO ()
testDifferentElementsCase = 
    testPost unCurriedIsPermutation (== False) differentElementsCase


-- Test if the reverse is also a permutation:
-- i.e. if a is a permuation of b, then b is a permutation of a.
-- Expectation: 'True'
testReversePermutationCase :: IO()
testReversePermutationCase =
    testPost 
        (\(a, b) -> (isPermutation a b) && (isPermutation b a))
        (== True) 
        permutationCase

-- A list with all the tests.
allPermutationTests = [
    testDifferentLengthCase,
    testPermutationCase,
    testDifferentElementsCase,
    testReversePermutationCase
    ]

-- Execute all the tests.
testAllPermutation :: IO ()
testAllPermutation = sequence_ allPermutationTests
