module Triangle where

import Data.List

data Shape = NoTriangle | Equilateral 
            | Isosceles  | Rectangular | Other deriving (Eq,Show)

isTriangle :: Integer -> Integer -> Integer -> Bool
isTriangle a b c | a <= 0 || b <= 0 || c <= 0 = False
                 | (sorted_sides !! 2 >= ((sorted_sides !! 1) + (sorted_sides !! 0))) = False
                 | otherwise = True 
    where
        sorted_sides = sort [a,b,c]

isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral a b c = a == b && b == c

isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c = (sorted_sides !! 2)^2 ==
                      (sorted_sides !! 0)^2 + (sorted_sides !! 1)^2
    where
        sorted_sides = sort [a,b,c]

isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles a b c = a == b || a == c || b == c

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | not (isTriangle a b c) = NoTriangle
               | isEquilateral a b c = Equilateral
               | isRectangular a b c = Rectangular
               | isIsosceles a b c = Isosceles
               | otherwise = Other 
