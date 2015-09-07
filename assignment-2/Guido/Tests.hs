module Lab2 where

import Data.List
import System.Random

type Triple a = (a,a,a)

limit :: Integer
limit = 1000

shuffleTriple :: Triple Integer -> IO (Triple Integer)
shuffleTriple (a,b,c) = do
    x   <- randomRIO (0, 5)
    abc <- return $ permutations [a,b,c] !! x
    return (abc !! 0, abc !! 1, abc !! 2)

randomRangeWithExclusions :: Integer -> Integer -> [Integer]
                          -> IO Integer
randomRangeWithExclusions lower upper exclusions = do
    x <- randomRIO (0, length xs - 1) 
    return $ xs !! x
    where xs = [lower..upper] \\ exclusions

triangleTestCases :: Int -> IO [Triple Integer]
triangleTestCases 0 = return []
triangleTestCases n = do
    x    <- randomRIO (0, 1) :: IO Integer
    abc' <- correctTestCase
    abc  <- shuffleTriple abc'
    xs   <- triangleTestCases (n-1)
    return $ abc : xs

correctTestCase :: IO (Triple Integer)
correctTestCase = do
    x <- randomRIO (0, 5) :: IO Integer
    case x of
        0 -> negativeOrZeroSideCase
        1 -> longSideCase
        2 -> equilateralCase
        3 -> isoscelesCase
        4 -> rectangularCase
        5 -> otherCase

negativeOrZeroSideCase :: IO (Triple Integer)
negativeOrZeroSideCase = do
    a <- randomRIO (-10, 0)
    b <- randomRIO (1, limit)
    c <- randomRIO (1, limit)
    return (a,b,c)

longSideCase :: IO (Triple Integer)
longSideCase = do
    a <- randomRIO (1, limit)
    b <- randomRIO (1, limit)
    c <- randomRIO (a+b+1, a+b+1+limit)
    return (a,b,c)

equilateralCase :: IO (Triple Integer)
equilateralCase = do
    a <- randomRIO (1, limit)
    return (a,a,a)

isoscelesCase :: IO (Triple Integer)
isoscelesCase = do
    ab <- randomRIO (1, limit)
    c  <- randomRangeWithExclusions 1 (2*ab-1) [ab]
    return (ab, ab, c)

allRectangularCases :: [Triple Integer]
allRectangularCases = [(3,4,5),(5,12,13),(9,40,41)]

isRectangularCase :: Triple Integer -> Bool
isRectangularCase (a,b,c) = c'^2 == a'^2 + b'^2
    where [a',b',c'] = sort [a,b,c] 

rectangularCase :: IO (Triple Integer)
rectangularCase = do
    x <- randomRIO (0, length allRectangularCases - 1)
    return $ allRectangularCases !! x

otherCase :: IO (Triple Integer)
otherCase = do
    a <- randomRIO (1, limit)
    b <- randomRangeWithExclusions 1 limit [a]
    c <- randomRangeWithExclusions 1 (a+b-1) [a,b]
    if isRectangularCase (a,b,c) then otherCase else return (a,b,c)

main = putStrLn "Hello, world!" 
