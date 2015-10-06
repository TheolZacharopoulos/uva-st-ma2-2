module ExM where 

exM :: Integer -> Integer -> Integer -> Integer
exM _ 0 m = 1 `mod` m
exM b e m =  
  if odd e then
     (result * b) `mod` m
  else
    result
  where
    result = exM ((b * b) `mod` m) (e `div` 2) m
