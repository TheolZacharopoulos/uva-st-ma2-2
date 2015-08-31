getDigits :: Integer -> [Integer]
getDigits = map (read . return) . show

getRevDigits = reverse . getDigits

getInnerDigits :: [Integer] -> [[Integer]]
getInnerDigits x = map (getDigits) x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : 2 * y : doubleEveryOther xs
doubleEveryOther [] = []

sumDigits x = ( sum . concat . getInnerDigits . reverse . doubleEveryOther . getRevDigits ) x

luhn x = sumDigits x `rem` 10 == 0
