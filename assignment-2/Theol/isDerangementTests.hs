module IsDerangementTests where

import Lecture2Test
import IsDerangement

import Data.List
import System.Random

limit :: Integer
limit = 2

curriedIsDerangement (xs, ys) = isDerangement xs ys

getUniqueRandomList :: Integer -> IO ([Integer])
getUniqueRandomList 0 = do
    return []
getUniqueRandomList length = do
    rnd <- randomRIO(-limit, limit)
    newList <- getUniqueRandomList(length-1)
    return $ rnd : newList

--derangementCase :: IO ([Integer], [Integer])
--derangementCase = do
--	rndLengthXS <- randomRIO(1, limit)
--	xs <- getUniqueRandomList rndLengthXS
--	rndIndex <- randomRIO(0, (length xs)-1)
--	return (xs, permutations xs !! rndIndex)
--
--testDerangementCase :: IO ()
--testDerangementCase =
--	testPost curriedIsDerangement (==True) derangementCase

--
-- Generates a random lengths, it creates a new unique list with this length,
--
duplicateElementCase :: IO ([Integer], [Integer])
duplicateElementCase = do
    length <- randomRIO(1, limit)
    list1 <- getUniqueRandomList length
    let list2 = (list1 !! 1):(delete (list1 !! 2) list1)
    return (list1, list2)

-- testDuplicateElementCase =
--	testPost curriedIsDerangement (==False) duplicateElementCase

-- elementOnSameIndexCase :: IO ([Integer], [Integer])

-- testRlementOnSameIndexCase =
--	testPost curriedIsDerangement (==False) elementOnSameIndexCase