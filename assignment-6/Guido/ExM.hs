module ExM where

import Lecture6

exM :: Integer -> Integer -> Integer -> Integer
exM _ 0 m = 1 `mod` m
exM b e m = b' * b^(e-e') `mod` m
    where (e',b') = until (\(x,_) -> x*2 > e)
                          (\(x,y) -> (x*2,y^2 `mod` m))
                          (1,b `mod` m)
