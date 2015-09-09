module Iban where

import Data.Char
import Data.List

removeNonAlphaNum :: String -> String
removeNonAlphaNum = flip intersect (['a'..'z']++['A'..'Z']++['0'..'9'])

getCodeChar :: Char -> Int
getCodeChar c = (ord c - ord 'a') + 10

toLowerCase :: String -> String
toLowerCase = map toLower

-- rotate 4 "ABCDEFG" = "EFGABCD"
-- rotate (-2) "ABCDEFG" = "FGABCDE"
rotate :: Int -> [a] -> [a]
rotate x l =  (iterate f l) !! (x `mod` length l)
  where f []     = []
        f (x':xs) = xs ++ [x']

-- perform step 2 of chapter 6.1 of the spec
toNumber :: String -> Integer
toNumber = read . concatMap f
  where f :: Char -> String
        f c = if null (intersect [c] ['a'..'z'])
              then [c]
              else show (getCodeChar c)

iban :: String -> Bool
iban = (== 1) . (`mod` 97) . toNumber . rotate 4 . toLowerCase . removeNonAlphaNum
