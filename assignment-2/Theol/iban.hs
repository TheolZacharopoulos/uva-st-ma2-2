module Iban where

import Data.Char
import Data.List

removeNonAlphaNum :: String -> String
removeNonAlphaNum = flip intersect (['a'..'z']++['A'..'Z']++['0'..'9'])

getCodeChar :: Char -> Int
getCodeChar c = (ord c - ord 'a') + 10

toLowerCase :: String -> String
toLowerCase = map toLower

--iban :: String -> Bool
--iban = 