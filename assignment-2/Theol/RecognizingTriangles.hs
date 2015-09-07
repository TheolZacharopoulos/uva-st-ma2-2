module RecognizingTriangles where
import Data.List

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | (a <= 0) || (b <= 0) || (c <= 0) = NoTriangle 
               | sorted_a >= sorted_b + sorted_c  = NoTriangle 
               | (a == b) && (b == c)             = Equilateral 
               | (a == b) || (b == c)             = Isosceles
               | (a^2 + b^2) == c^2               = Rectangular
               | otherwise                        = Other
    where 
        [sorted_a, sorted_b, sorted_c] = reverse $ sort [a,b,c]
