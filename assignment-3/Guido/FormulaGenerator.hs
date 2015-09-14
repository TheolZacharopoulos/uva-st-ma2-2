module One where

import Lecture3

import System.Random

randomAtomicProp :: (Name, Name) -> IO Form
randomAtomicProp range = do
    n <- randomRIO range
    return $ Prop n

randomForms :: (Name, Name) -> Int -> IO [Form]
randomForms range n | n == 1    = do
                                    f <- randomAtomicProp range
                                    return [f]
                    | otherwise = do
                                    m  <- randomRIO (1, n-1) :: IO Int
                                    f  <- randomForm range m
                                    fs <- randomForms range (n-m)
                                    return $ f : fs

randomForm :: (Name, Name) -> Int -> IO Form
randomForm range n | n == 1 = do
    m <- randomRIO (0,2) :: IO Int
    case m of
        0 -> randomAtomicProp range
        1 -> return $ Cnj []
        2 -> return $ Dsj []

                   | n == 2 = do
    m <- randomRIO (0,2) :: IO Int
    f <- randomAtomicProp range
    case m of
        0 -> return $ Neg f 
        1 -> return $ Cnj [f]
        2 -> return $ Dsj [f]

                   | otherwise = do 
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
