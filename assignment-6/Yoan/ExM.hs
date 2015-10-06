module ExM where

import Data.List
import System.Random
import Lecture6

exMRecursive :: Integer -> Integer -> Integer -> Integer -> Integer
exMRecursive base exponent modulus result =
    if exponent > 0 then
        exMRecursive newBase newExponent modulus newResult
    else
        result
    where
        newBase = (base * base) `mod` modulus
        newExponent = exponent `div` 2
        newResult =
            if (exponent `mod` 2) == 1 then ((result * base) `mod` modulus)
            else result


myExM :: Integer -> Integer -> Integer -> Integer
myExM base exponent modulus = exMRecursive base exponent modulus 1