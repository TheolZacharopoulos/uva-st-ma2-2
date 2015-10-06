module Lecture6_1
  
where 

modularExp :: Integer -> Integer -> Integer -> Integer
modularExp _ 0 m = 1 `mod` m
modularExp b e m =  
  if odd e then
     (result * b) `mod` m
  else
    result
  where
    result = (modularExp ((b * b) `mod` m) (e `div` 2) m)


expM ::  Integer -> Integer -> Integer -> Integer
expM x y = rem (x^y)