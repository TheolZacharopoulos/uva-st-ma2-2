module TriangleTests where

import Lecture2Test
import Triangle

import Data.List
import System.Random

type Triple a = (a,a,a)

limit :: Integer
limit = 10000

-- Helper function
-- Shuffles the arguments a,b,c for randomizing the function input order.
shuffle :: IO (Triple Integer) -> IO (Triple Integer)
shuffle gen = do
    (a,b,c) <- gen
    rndIndex <- randomRIO (0, 5)
    abc      <- return $ permutations [a,b,c] !! rndIndex
    return (abc !! 0, abc !! 1, abc !! 2)

-- Helper function
-- Generates a random Integer between `lower` and `upper` limit, excluding a list of integers `exclusions`
randomRangeWithExclusions :: Integer -> Integer -> [Integer] -> IO Integer
randomRangeWithExclusions lower upper exclusions = do
    x <- randomRIO (lower, upper)
    if length (intersect [x] exclusions) == 1
      then randomRangeWithExclusions lower upper exclusions
      else return x

-- Helper function
-- Checks the validity of a triangle according to the theory.
isAValidTriangle :: Triple Integer -> Bool
isAValidTriangle (x, y, z) = x + y > z && x + z > y && z + y > x

-- Helper function
-- Calculates possible rectangular triangles.
calcPossibleRectCases :: Triple Integer -> [Triple Integer] -> Integer -> [Triple Integer]
calcPossibleRectCases (a, b, c) theRest n = do
    if a*n > limit || b*n > limit || c*n > limit then
        theRest
    else
        calcPossibleRectCases (a, b, c) (theRest ++ [(a*n, b*n, c*n)]) (n+1)

-- Helper function
-- Returns a list of the rectangultar triangles base cases, according to the theory.
allRectangularBaseCases :: [Triple Integer]
allRectangularBaseCases = [(3,4,5),(5,12,13),(9,40,41)]

-- Helper function
-- Caclulates of rectangular triangle cases.
allRectangularCases :: [Triple Integer]
allRectangularCases = concatMap (\t -> calcPossibleRectCases t [] 1) allRectangularBaseCases

-- Helper function
-- Checks if it is a rectangular case.
isRectangularCase :: Triple Integer -> Bool
isRectangularCase (a,b,c) = c'^2 == a'^2 + b'^2
    where [a',b',c'] = sort [a,b,c]

-- Helper function
-- Curies a triangle.
curriedTriangle :: Triple Integer -> Shape
curriedTriangle (a,b,c) = triangle a b c

-- This function generates a negative or zero input case.
-- A triangle which has at least one side that is equal or less than 0 is not a triangle.
negativeOrZeroSideCase :: IO (Triple Integer)
negativeOrZeroSideCase = do
    a <- randomRIO (-10, 0)
    b <- randomRIO (1, limit)
    c <- randomRIO (1, limit)
    return (a,b,c)

-- This function generates a side that is bigger than the sum of the other two.
-- A triangle which has at one side which is longer than the sum of the other two is not a triangle.
longSideCase :: IO (Triple Integer)
longSideCase = do
    a <- randomRIO (1, limit)
    b <- randomRIO (1, limit)
    c <- randomRIO (a+b+1, a+b+1+limit)
    return (a,b,c)

-- This function generates a case which every side has the same length.
-- A triangle which has the same length for every side is an equilateral triangle.
equilateralCase :: IO (Triple Integer)
equilateralCase = do
    a <- randomRIO (1, limit)
    return (a,a,a)

-- This function generates a case which 2 of the sides are the same, the 3rd one is always different (isosceles triangle).
-- A triangle which has the same length for 2 of it's sides is an isosceles triangle.
isoscelesCase :: IO (Triple Integer)
isoscelesCase = do
    ab <- randomRIO (1, limit)
    c  <- randomRangeWithExclusions 1 (2*ab-1) [ab]
    return (ab, ab, c)

-- This function generates a case which consist of a valid rectangular triangle (rectangular triangle).
-- Uses the helper functions to generate a valid rectangular case.
rectangularCase :: IO (Triple Integer)
rectangularCase = do
    x <- randomRIO (0, length allRectangularCases - 1)
    shuffle (return $ allRectangularCases !! x)

-- This function generates a case that is not consist of any other above cases (other).
otherCase :: IO (Triple Integer)
otherCase = do
    a <- randomRIO (1, limit)
    b <- randomRangeWithExclusions 1 limit [a]
    c <- randomRangeWithExclusions 1 (a+b-1) [a,b]
    if isRectangularCase (a,b,c) || not (isAValidTriangle (a,b,c)) then otherCase else return (a,b,c)

-- Test the negative or zero case.
-- Input: one of the arguments as zero or negative (invalid triangle).
-- Expectation: 'NoTriangle'.
testNegativeOrZeroSideCase :: IO ()
testNegativeOrZeroSideCase =
    testPost curriedTriangle (== NoTriangle) negativeOrZeroSideCase

-- Test the long side case.
-- Input: one of the arguments is bigger than the sum of the other two (invalid triangle).
-- Expectation: 'NoTriangle'.
testLongSideCase :: IO ()
testLongSideCase =
    testPost curriedTriangle (== NoTriangle) longSideCase

-- Test the equilateral triangle case.
-- Input: every argument is the same (equilateral triangle).
-- Expectation: 'Equilateral'
testEquilateralCase :: IO ()
testEquilateralCase =
    testPost curriedTriangle (== Equilateral) equilateralCase

-- Test the rectangular triangle case.
-- Input: a valid rectangular triangle.
-- Expectation: 'Rectangular'
testRectangularCase :: IO ()
testRectangularCase =
    testPost curriedTriangle (== Rectangular) rectangularCase

-- Test the isosceles triangle case.
-- Input: 2 arguments are the same, the 3rd is different (isosceles triangle).
-- Expectation: 'Isosceles'
testIsoscelesCase :: IO ()
testIsoscelesCase =
    testPost curriedTriangle (== Isosceles) isoscelesCase

-- Test the other case.
-- Input: a random generated other case, which it's not consists of any of the previous cases.
-- Expectation: 'Other'
testOtherCase :: IO ()
testOtherCase =
    testPost curriedTriangle (== Other) otherCase

-- A list with all the tests.
allTriangleTests = [
    testNegativeOrZeroSideCase,
    testLongSideCase,
    testEquilateralCase,
    testIsoscelesCase,
    testRectangularCase,
    testOtherCase]

-- Execute all the triangle tests.
testAllTriangles :: IO [()]
testAllTriangles = sequence allTriangleTests