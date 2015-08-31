module Luhn (main) where

listify :: Integer -> [Integer]
listify x | x == xmod10 = [x]
          | otherwise   = xmod10 : listify xdiv10
  where (xdiv10, xmod10) = x `divMod` 10

luhn :: Integer -> Bool
luhn = (== 0) . (`mod` 10) . sum . concat . map listify . zipWith (*) (cycle [1,2]) . listify

main :: IO ()
main = putStrLn $ show $ luhn 378282246310005
