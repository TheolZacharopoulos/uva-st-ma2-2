module Composites where

import Data.List
import System.Random

progressiveIsPrime :: Integer -> Integer -> Bool
progressiveIsPrime current supposed =
    if current*current <= supposed then
        if (supposed `mod` current) == 0 || (supposed `mod` (current + 2)) == 0 then
            False
        else
            progressiveIsPrime (current + 6) supposed
    else
        True

isPrime :: Integer -> Bool
isPrime number =
    if number <= 1 then
        False
    else if number <= 3 then
        True
    else if  (number `mod` 2) == 0 || (number `mod` 3) == 0 then
        False
    else
        progressiveIsPrime 5 number

composites :: [Integer]
composites = filter (not . isPrime) [1..]

compositesRange :: Integer -> [Integer]
compositesRange limit = filter (not . isPrime) [1..limit]