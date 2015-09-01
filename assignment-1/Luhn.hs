module Luhn where

{-# DEPRECATED lastDigit, dropLastDigit "! Knowingly not used, only for tests !" #-}

-- Convert a number to a list
numToList :: Integer -> [Integer]
numToList = map (\x -> read [x] :: Integer) . show

-- Convert a list to a number
listToNum :: [Integer] -> Integer
listToNum = foldl listToNum 0 where listToNum num d = 10 * num + d

-- ! Knowingly not used, only for tests !
-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = last . numToList

-- ! Knowingly not used, only for tests !
-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = listToNum . reverse . drop 1 . toRevDigits

-- Break apart a number into its digits, in reverse.
toRevDigits :: Integer -> [Integer]
toRevDigits n | n <= 0    = []
              | otherwise = reverse.numToList $ n

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) (cycle [1,2])

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map numToList

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toRevDigits
