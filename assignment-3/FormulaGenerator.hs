module FormulaGenerator (randomForm) where

import System.Random

import Lecture3

-- randomForm (x,y) n: create a random formula consisting of n random
-- sub-formulas (including possible duplicate occurrences), where each atomic
-- proposition has an index between x and y inclusive.
randomForm :: (Name, Name) -> Int -> IO Form

randomForm range 1 = do
    m <- randomRIO (0,4) :: IO Int
    case m of
        0 -> randomAtomicProp range
        1 -> return $ Cnj [] -- evl [] (Cnj []) = True
        2 -> return $ Dsj [] -- evl [] (Dsj []) = False
        3 -> return T
        4 -> return F

randomForm range 2 = do
    m <- randomRIO (0,2) :: IO Int
    f <- randomForm range 1
    case m of
        0 -> return $ Neg f 
        1 -> return $ Cnj [f]
        2 -> return $ Dsj [f]

randomForm range n = do
    m <- randomRIO (0,4) :: IO Int
    case m of
        0 -> do
            f <- randomForm range (n-1)
            return $ Neg f 
        1 -> do
            fs <- randomForms range (n-1)
            return $ Cnj fs
        2 -> do
            fs <- randomForms range (n-1)
            return $ Dsj fs
        3 -> do
            f1 <- randomForm range (n `div` 2)
            f2 <- randomForm range (n `div` 2 + n `rem` 2 - 1)
            return $ Impl f1 f2
        4 -> do
            f1 <- randomForm range (n `div` 2)
            f2 <- randomForm range (n `div` 2 + n `rem` 2 - 1)
            return $ Equiv f1 f2


randomAtomicProp :: (Name, Name) -> IO Form
randomAtomicProp range = do
    n <- randomRIO range
    return $ Prop n


randomForms :: (Name, Name) -> Int -> IO [Form]
randomForms range 1 = do
    f <- randomForm range 1
    return [f]

randomForms range n = do
    m  <- randomRIO (1, n-1) :: IO Int
    f  <- randomForm range m
    fs <- randomForms range (n-m)
    return $ f : fs
