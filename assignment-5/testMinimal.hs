module TestMinimal where

import Lecture5
import System.Random

-- Finds a random filled position
getRandomFilledPosition :: Node -> IO (Row, Column)
getRandomFilledPosition node = do
    let filled = filledPositions $ fst node
    rndIndex <- randomRIO(0, (length filled))
    let rndPos = (filled !! rndIndex)
    return rndPos

-- Removes a random filled position (Hint)
removeRandomHint :: Node -> IO (Node)
removeRandomHint node = do
    randomPosition <- getRandomFilledPosition node
    return (eraseN node randomPosition)

-- 1. Generate a sudoku problem P.
-- 2. Use the uniqueSol function on P, should give True.
-- 3. Create a problem P' by erasing one of the hints given in P,
--    this should give more than one solutions, 
-- 	  so not minimal, so it should give False.
testIsMinimal :: IO (Bool)
testIsMinimal = do
	rndSudoku <- genRandomSudoku 	-- Get a random solved sudoku
	rndP  <- genProblem rndSudoku 	-- Get a random sudoku problem
	rndP' <- removeRandomHint rndP	-- Remove a (random) hint from P. 
	let isMinimal    = uniqueSol rndP
	let isNotMinimal = uniqueSol rndP'
	return ((isMinimal) && (not isNotMinimal)) -- isMinimal should be true, isNotMinimal should be False

numOfTests :: Int
numOfTests = 5

-- Run the tests n times.
runTests :: IO ()
runTests = do
    test <- testIsMinimal
    sequence_ $ take numOfTests $ repeat $ (if test then putStrLn "Success" else putStrLn "Fail")