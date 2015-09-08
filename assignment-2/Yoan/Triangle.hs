module Triangle where

import Data.List

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show,Ord)

-- Find if the provided sizes are from a real triangle
isTriangle :: Integer -> Integer -> Integer -> Bool
isTriangle x y z = x + y > z && x + z > y && z + y > x

-- Check if the provides sizes indicate equilateral triangle
isEquilateralTriangle :: Integer -> Integer -> Integer -> Bool
isEquilateralTriangle x y z = x == y && y == z && z == x

findHypotenuse :: Integer -> Integer -> Integer -> Integer
findHypotenuse x y z = maximum ([x, y, z])

findCathetuses :: Integer -> Integer -> Integer -> [Integer]
findCathetuses x y z = tail (reverse (sort [x, y, z]))

-- Check if the triangle is rectengural one
isRightTriangle :: Integer -> Integer -> Integer -> Bool
isRightTriangle x y z = (findHypotenuse x y z) ^ 2
                      == sum [(cathetus^2) | cathetus <- findCathetuses x y z]

-- Check if the triangle has isosceles sides
isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles x y z = (x == y || x == z || y == z) && not (isEquilateralTriangle x y z)

-- Find what is the type of the triangle
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
        | not (isTriangle x y z) = NoTriangle
        | isIsosceles x y z = Isosceles
        | isEquilateralTriangle x y z = Equilateral
        | isRightTriangle x y z = Rectangular
        | otherwise = Other
