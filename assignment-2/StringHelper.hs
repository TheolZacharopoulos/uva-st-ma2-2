module StringHelper where

import Data.Char
import Data.List

removeNonAlphaNum :: String -> String
removeNonAlphaNum = flip intersect alphaNum

getCodeChar :: Char -> Int
getCodeChar c = (ord c - ord 'a') + 10

toLowerCase :: String -> String
toLowerCase = map toLower

alpha :: String
alpha = ['A'..'Z']++['a'..'z']

num :: String
num = ['0'..'9']

alphaNum :: String
alphaNum = num++alpha

