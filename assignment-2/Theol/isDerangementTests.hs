module IsDerangementTests where

import Lecture2Test
import IsDerangement

import System.Random

limit :: Integer
limit = 10000

curriedIsDerangement (xs, ys) = isDerangement xs ys

--getUniqueRandomList :: Integer -> IO([Integer])
--getUniqueRandomList 0 = do
--	return []
--getRandomList x = do
--	r <- randomRIO(-limit, limit)
--	l <- getRandomList (x-1)
--	return $ [r] ++ l

--getRandomList :: Integer -> IO([Integer])
--getRandomList 0 = do
--	return []
--getRandomList x = do
--	r <- randomRIO(-limit, limit)
--	l <- getRandomList (x-1)
--	return $ [r] ++ l

--derangementCase :: IO ([Integer], [Integer])
--derangementCase = do
--	rndLengthXS <- randomRIO(1, limit)
--	xs <- getUniqueRandomList rndLengthXS
--	ys <- 

--testDerangementCase = 
--	testPost curriedIsDerangement (==True) derangementCase

--notDerangementCase :: IO ([Integer], [Integer])

--testNotDerangementCase = 
--	testPost curriedIsDerangement (==False) derangementNotCase