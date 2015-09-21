module SetInt where

import Control.Monad
import SetOrd
import System.Random
import Data.List
import Data.Array.IO

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


getRandomSetInt :: Int -> IO (Set Int)
getRandomSetInt limit = do
    len <- randomRIO(0, limit)
    shuffled <- shuffle [0..len]
    return $ Set shuffled

--getRandomSetInt :: Int -> IO (Set Int)
--getRandomSetInt limit = do
--    l <- randomRIO(0, limit)
--    s <- getRandomSetIntR l limit
--    return s


-- Generates a random Integer list.
--getRandomSetIntR :: Int -> Int -> IO (Set Int)
--getRandomSetIntR 0 _ = do
--    return $ Set []
--getRandomSetIntR x limit = do
--    r <- randomRIO (-limit, limit)
--    l <- getRandomSetIntR (x - 1) limit
--    return $ r `insertSet` l
