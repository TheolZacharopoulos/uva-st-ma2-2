module LeastFalsePositive where

import Control.Monad
import Lecture6

least_false_positive :: (Int -> Integer -> IO Bool)
                     -> (Integer -> Int) -> [Integer] -> Int -> IO Integer
least_false_positive f fi (x:xs) k = do
    ts <- sequence $ take (fi x) $ repeat $ f k x
    if any id ts
    then return x
    else least_false_positive f fi xs k
