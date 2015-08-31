module Main (main) where

add_and_multiply :: Integer -> Integer
add_and_multiply 0 = 0
add_and_multiply n = (add (n `div` 10)) + (rem n 10) * 2

add :: Integer -> Integer
add 0 = 0
add n = add_and_multiply (n `div` 10) + rem n 10

luhn :: Integer -> Bool
luhn n = (mod (add n) 10) == 0

main :: IO ()
main = putStrLn $ show $ luhn 505
