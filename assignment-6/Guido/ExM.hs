module ExM where

import Lecture6

exM :: Integer -> Integer -> Integer -> Integer
exM _ 0 _ = 1
exM b e m = exMsq 1 (b `mod` m)
    where exMsq e' b' = let e'2 = e'*2
                        in  if e'2 <= e
                            then exMsq e'2 (b'*b' `mod` m)
                            else b' * (b^(e-e') `mod` m)
