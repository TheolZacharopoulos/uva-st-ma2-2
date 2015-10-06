module Lecture6_4 where

import Control.Monad
import Composites
import Lecture6

least_false_positive_F :: Int -> IO Integer
least_false_positive_F = f composites
    where f (x:xs) k = do
            -- prime_tests_F randomizes on the moduli of its tests.
            -- therefore applying it only once may miss the case where
            -- a chosen modulus results in a false positive.
            -- To increase the chances that all the moduli are tested
            -- the test is run multiple times.
            ts <- sequence $ take (fromInteger x*1000) $ repeat $ prime_tests_F k x
            if any id ts
            then return x
            else f xs k

least_false_positives_F :: [Int] -> IO [Integer]
least_false_positives_F = mapM least_false_positive_F

main :: IO ()
main = do lfp <- least_false_positives_F [1..3]
          print lfp
