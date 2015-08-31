module Lab1 (main) where

listify :: Integer -> [Integer]
listify x = (x `mod` 10) : tail
  where tail = if x == x `mod` 10 then [] else listify (x `div` 10)

luhn :: Integer -> Bool
luhn = (== 0) . (`mod` 10) . sum . (concat . map listify) . zipWith (*) (cycle [1,2]) . listify

main :: IO ()
main = putStrLn $ show $ luhn 378282246310005
