module ExM where

import Lecture6

exM :: Integer -> Integer -> Integer -> Integer
exM _ 0 m = 1 `mod` m
exM b e m = exMsq 1 (b `mod` m)
    where exMsq e' b' = let e2' = e'*2
                        in  if e2' <= e
                            then exMsq e2' (b'*b' `mod` m)
                            else b' * b^(e-e') `mod` m
