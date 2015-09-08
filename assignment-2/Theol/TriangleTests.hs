module TriangleTests where

import Lecture2Test
import Triangle

import Data.List
import System.Random

type Triple a = (a,a,a)

limit :: Integer
limit = 10000

shuffle :: IO (Triple Integer) -> IO (Triple Integer)
shuffle gen = do
    (a,b,c) <- gen
    x       <- randomRIO (0, 5)
    abc     <- return $ permutations [a,b,c] !! x
    return (abc !! 0, abc !! 1, abc !! 2)

curriedTriangle :: Triple Integer -> Shape
curriedTriangle (a,b,c) = triangle a b c

negativeOrZeroSideCase :: IO (Triple Integer)
negativeOrZeroSideCase = do
    a <- randomRIO (-10, 0)
    b <- randomRIO (1, limit)
    c <- randomRIO (1, limit)
    return (a,b,c)

testNegativeOrZeroSideCase :: IO ()
testNegativeOrZeroSideCase =
    testPost curriedTriangle (== NoTriangle) negativeOrZeroSideCase

longSideCase :: IO (Triple Integer)
longSideCase = do
    a <- randomRIO (1, limit)
    b <- randomRIO (1, limit)
    c <- randomRIO (a+b+1, a+b+1+limit)
    return (a,b,c)

testLongSideCase :: IO ()
testLongSideCase =
    testPost curriedTriangle (== NoTriangle) longSideCase

equilateralCase :: IO (Triple Integer)
equilateralCase = do
    a <- randomRIO (1, limit)
    return (a,a,a)

testEquilateralCase :: IO ()
testEquilateralCase =
    testPost curriedTriangle (== Equilateral) equilateralCase

randomRangeWithExclusions :: Integer -> Integer -> [Integer]
                          -> IO Integer
randomRangeWithExclusions lower upper exclusions = do
    x <- randomRIO (lower, upper)
    if length (intersect [x] exclusions) == 1
      then randomRangeWithExclusions lower upper exclusions
      else return x

isoscelesCase :: IO (Triple Integer)
isoscelesCase = do
    ab <- randomRIO (1, limit)
    c  <- randomRangeWithExclusions 1 (2*ab-1) [ab]
    return (ab, ab, c)

testIsoscelesCase :: IO ()
testIsoscelesCase =
    testPost curriedTriangle (== Isosceles) isoscelesCase

calcPossibleRectCases :: Triple Integer -> [Triple Integer] -> Integer -> [Triple Integer]
calcPossibleRectCases (a, b, c) theRest n = do
    if a*n > limit || b*n > limit || c*n > limit then
        theRest
    else
        calcPossibleRectCases (a, b, c) (theRest ++ [(a*n, b*n, c*n)]) (n+1)

allRectangularBaseCases :: [Triple Integer]
allRectangularBaseCases = [(3,4,5),(5,12,13),(9,40,41)]

allRectangularCases :: [Triple Integer]
allRectangularCases = concatMap (\t -> calcPossibleRectCases t [] 1) allRectangularBaseCases

isRectangularCase :: Triple Integer -> Bool
isRectangularCase (a,b,c) = c'^2 == a'^2 + b'^2
    where [a',b',c'] = sort [a,b,c]

rectangularCase :: IO (Triple Integer)
rectangularCase = do
    x <- randomRIO (0, length allRectangularCases - 1)
    return $ allRectangularCases !! x

testRectangularCase :: IO ()
testRectangularCase =
    testPost curriedTriangle (== Rectangular) rectangularCase

isAValidTriangle :: Triple Integer -> Bool
isAValidTriangle (x, y, z) = x + y > z && x + z > y && z + y > x

otherCase :: IO (Triple Integer)
otherCase = do
    a <- randomRIO (1, limit)
    b <- randomRangeWithExclusions 1 limit [a]
    c <- randomRangeWithExclusions 1 (a+b-1) [a,b]
    if isRectangularCase (a,b,c) || not (isAValidTriangle (a,b,c)) then otherCase else return (a,b,c)

testOtherCase :: IO ()
testOtherCase =
    testPost curriedTriangle (== Other) otherCase
