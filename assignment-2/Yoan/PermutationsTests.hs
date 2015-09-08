module PermutationsTest where

import System.Random
import Shuffle
import PermutationsCheck
import Lecture2Test

curriedIsPermutation :: Eq a => ([a], [a]) -> Bool
curriedIsPermutation (l1, l2) = isPermutation l1 l2

-- Constants
randomStartRange :: Integer
randomStartRange = 100

randomEndRange :: Integer
randomEndRange = 999

-- Helpers
getDigits :: Integer -> [Integer]
getDigits = map (read . return) . show

-- Generates a random list of numbers
randomNumberList :: IO ([Integer])
randomNumberList = do
    randomNumber <- randomRIO(randomStartRange, randomEndRange)
    return (getDigits randomNumber)

randomPermutationCase :: IO ([Integer], [Integer])
randomPermutationCase = do
    randomized <- randomNumberList
    shuffled <- shuffle randomized
    return (shuffled, randomized)

modify :: Integer -> IO (Integer)
modify x = do
    randomValue <- randomRIO(randomStartRange, randomEndRange)
    return (x * randomValue)

modifiedPermutationCase :: IO ([Integer], [Integer])
modifiedPermutationCase = do
    randomized <- randomNumberList
    shuffled <- shuffle randomized
    modified <- (mapM (modify) shuffled)
    return (randomized, modified)

extend :: [Integer] -> IO ([Integer])
extend x = do
    randomValue <- randomNumberList
    return (x ++ randomValue)

extendedPermutationCase :: IO ([Integer], [Integer])
extendedPermutationCase = do
    randomized <- randomNumberList
    shuffled <- shuffle randomized
    extended <- extend shuffled
    return (randomized, extended)

testRandomPermutationsCase :: IO ()
testRandomPermutationsCase = do
    testPost curriedIsPermutation (== True) randomPermutationCase

testWrongPermutationsCase :: IO ()
testWrongPermutationsCase = do
    testPost curriedIsPermutation (== False) modifiedPermutationCase

testDiffLengthPermutationsCase :: IO ()
testDiffLengthPermutationsCase = do
    testPost curriedIsPermutation (== False) extendedPermutationCase