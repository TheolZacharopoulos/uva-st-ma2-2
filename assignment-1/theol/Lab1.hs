module Lab1 (main) where 

as_list 0 = []
as_list x = as_list (x `div` 10) ++ [x `mod` 10]

take_every_start1 n = map snd . filter ((== n) . fst) . zip (cycle [1..n])
take_every_start2 n = map snd . filter ((== n) . fst) . zip (cycle [2..n+1])

mult2_2nds n = (map (*2) (take_every_start1 2 (reverse (as_list n))))

sum2nds n = concat (map as_list (mult2_2nds n))
sumRest n = [sum (take_every_start2 2 (reverse (as_list n)))]

total n = sum (sum2nds n ++ sumRest n)

luhn :: Integer -> Bool
luhn n = ((total n) `mod` 10) == 0

main :: IO ()
main = putStrLn $ show $ luhn 378282246310005
