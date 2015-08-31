getDigits :: Integer -> [Integer]
getDigits = map (read . return) . show

getInnerDigits :: [Integer] -> [[Integer]]
getInnerDigits x = map (getDigits) x

getRevDigits x = (reverse . getDigits) x

lastDigit x = (last . getDigits) x

dropLastDigit x = (reverse . (drop 1) . getRevDigits) x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : 2 * y : doubleEveryOther xs
doubleEveryOther [] = []

sumDigits x = ( sum . concat . getInnerDigits . reverse . doubleEveryOther . getRevDigits ) x

luhn x = sumDigits x `rem` 10 == 0
