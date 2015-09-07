module Lab2 where

import Data.List
import System.Random

type Triple a = (a,a,a)

shuffleTriple :: Triple Integer -> IO (Triple Integer)
shuffleTriple (a,b,c) = do
    x   <- randomRIO (0, 5)
    abc <- return $ permutations [a,b,c] !! x
    return (abc !! 0, abc !! 1, abc !! 2)

triangleTestCases :: Int -> IO [Triple Integer]
triangleTestCases 0 = return []
triangleTestCases n = do
    x    <- randomRIO (0, 1) :: IO Integer
    abc' <- if x == 0 then randomTestCase else correctTestCase
    abc  <- shuffleTriple abc'
    xs   <- triangleTestCases (n-1)
    return $ abc : xs

randomTestCase :: IO (Triple Integer)
randomTestCase = do
    a <- randomRIO (-100, 900)
    b <- randomRIO (-100, 900)
    c <- randomRIO (-100, 900)
    return (a,b,c)

correctTestCase :: IO (Triple Integer)
correctTestCase = do
    x <- randomRIO (0, 5) :: IO Integer
    case x of
        0 -> negativeOrZeroSideCase
        1 -> longSideCase
        2 -> equilateralCase
--        3 -> isoscelesCase
--        4 -> rectangularCase
--        5 -> otherCase

negativeOrZeroSideCase :: IO (Triple Integer)
negativeOrZeroSideCase = do
    a <- randomRIO (-10, 0)
    b <- randomIO
    c <- randomIO
    return (a,b,c)

longSideCase :: IO (Triple Integer)
longSideCase = do
    a <- randomIO
    b <- randomIO
    c <- randomRIO (a+b+1, a+b+1000)
    return (a,b,c)

equilateralCase :: IO (Triple Integer)
equilateralCase = do
    a <- randomRIO (1, 1000)
    return (a,a,a)

{-
isoscelesCase :: IO (Triple Integer)
isoscelesCase = do

rectangularCase :: IO (Triple Integer)
rectangularCase = do

otherCase :: IO (Triple Integer)
otherCase = do
-}

main = putStrLn "Hello, world!" 
