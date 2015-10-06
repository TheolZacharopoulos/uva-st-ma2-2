module Carmichael where

import Lecture6

carmichael :: [Integer]
carmichael = [(6*k+1)*(12*k+1)*(18*k+1)
             | k <- [2..]
             , isPrime (6*k+1)
             , isPrime (12*k+1) 
             , isPrime (18*k+1)]
