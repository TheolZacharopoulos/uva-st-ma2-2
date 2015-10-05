module Lecture6
  
where 

modularExp :: Integer -> Integer -> Integer -> Integer
modularExp _ 0 _ = 1
modularExp b e m =  
  if e `mod` 2 == 1 then
     (result * b) `mod` m
  else
    result
  where
    result = (modularExp ((b * b) `mod` m) (e `div` 2) m)


expM ::  Integer -> Integer -> Integer -> Integer
expM x y = rem (x^y)
