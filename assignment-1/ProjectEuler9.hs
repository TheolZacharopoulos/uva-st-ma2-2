module ProjectEuler9 (main) where

type Triple a = (a,a,a)

lowC :: Integer -> Integer
lowC x = d+m
  where (d,m) = x `divMod` 3

domainABC :: Integer -> Integer -> [Triple Integer]
domainABC x c = (a,b,c) : nextABC x (a,b,c)
  where b = min (x-c-1) (c-1)
        a = x-b-c

nextABC :: Integer -> Triple Integer -> [Triple Integer]
nextABC x (a,b,c) | a+1 < b-1 = (a+1,b-1,c) : nextABC x (a+1,b-1,c)
-- aa+bb < (a+b)^2, a+b = x-c
-- =>
-- aa+bb < (x-c)^2
-- =>
-- for c >= x-c there is no pythagorean triplet
                  | c+1 < x-c = domainABC x (c+1) 
                  | otherwise = []

isPythagoreanTriplet :: Triple Integer -> Bool
isPythagoreanTriplet (a,b,c) = a*a + b*b == c*c -- && a < b && b < c
    -- domainABC only produces triplets where a < b < c
    -- TODO: try to prove claim

pythagoreanTriplets :: Integer -> [Triple Integer]
pythagoreanTriplets x = filter isPythagoreanTriplet $ domainABC x (lowC x)

main :: IO ()
main = putStrLn $ show $ (\(a,b,c) -> a*b*c) $ head $ pythagoreanTriplets 1000
