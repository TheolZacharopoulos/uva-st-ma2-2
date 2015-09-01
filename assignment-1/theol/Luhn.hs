module Luhn where

-- Convert a number to a list
numToList :: Integer -> [Integer]
numToList 0 = []
numToList x = numToList (x `div` 10) ++ [x `mod` 10]

-- Convert a list to a number
listToNum :: [Integer] -> Integer
listToNum = foldl listToNum 0 where listToNum num d = 10 * num + d

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit 0 = 0
lastDigit n = last.numToList $ n

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = listToNum . reverse . drop 1 . toRevDigits

-- Break apart a number into its digits, in reverse.
toRevDigits :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits n = reverse.numToList $ n

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) $ cycle $ [1,2]

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ concat $ map numToList xs

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toRevDigits
