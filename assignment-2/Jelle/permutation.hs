module Triangle (main) where

import Data.List
import System.Random
import Data.Array.IO
import Control.Monad

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation l1 l2 = length l1 == length l2 && length (intersect l1 l2) == length l1


limit :: Integer
limit = 1000

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

getRandomList :: Integer -> IO [Integer]
getRandomList 0 = do
    return []
getRandomList x = do
    r <- randomRIO(-limit, limit)
    l <- getRandomList (x - 1)
    return $ [r] ++ l

differentLengthCase :: IO ([Integer], [Integer])
differentLengthCase = do
    length1 <- randomRIO (1, limit)
    length2 <- randomRIO (1, length1 - 1)
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
    length1 <- randomRIO(1, limit)
    l1 <- getRandomList length1
    l2 <- getRandomList length1
    return (l1, l2)

main :: IO ()
main = do
    putStrLn $ "Is permutation? " ++ (show $ isPermutation [1,3,2] [2,3,1])
    l <- getRandomList 5
    putStrLn $ "Random list? " ++ (show $ l)
    twoL <- differentLengthCase
    putStrLn $ "differentLengthCase? " ++ (show $ twoL)