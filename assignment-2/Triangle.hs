module Triangle where

import Data.List

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | sorted_c <= 0        		      		 = NoTriangle 
               | sorted_a >= sorted_b + sorted_c  		 = NoTriangle 
               | (a == b) && (b == c)             		 = Equilateral 
               | (a == b) || (b == c)             		 = Isosceles
               | (sorted_c^2 + sorted_b^2) == sorted_a^2 = Rectangular
               | otherwise                        		 = Other
    where
        [sorted_a, sorted_b, sorted_c] = reverse $ sort [a,b,c]