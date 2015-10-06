module LeastFalsePositive where

import Control.Monad
import Lecture6

least_false_positive_F :: Int -> [Integer] -> Int -> IO Integer
least_false_positive_F i (x:xs) k = do
    -- prime_tests_F randomizes on the moduli of its tests.
    -- therefore applying it only once may miss the case where
    -- a chosen modulus results in a false positive.
    -- To increase the chances that all the moduli are tested
    -- the test is run multiple times.
    ts <- sequence $ take (fromInteger x * i) $ repeat $ prime_tests_F k x
    if any id ts
    then return x
    else least_false_positive_F i xs k
