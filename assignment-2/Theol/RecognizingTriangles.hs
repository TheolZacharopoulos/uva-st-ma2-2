module RecognizingTriangles where

import Data.List

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq, Show)

triangleSorted :: [Integer] -> Shape
triangleSorted [a,b,c] | (a <= 0) || (b <= 0) || (c <= 0) = NoTriangle 
                       | a >= b + c                       = NoTriangle 
                       | b == c                           = Equilateral 
                       | (a == b) && (b == a)             = Isosceles
                       | (a^2 + b^2) == c^2               = Rectangular
                       | otherwise                        = Other

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = triangleSorted $ reverse $ sort [a,b,c]