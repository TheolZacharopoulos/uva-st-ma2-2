module Relations where

import Data.Tuple
import Data.List

type Rel a = [(a,a)]

inverseRel :: Ord a => Rel a -> Rel a
inverseRel = sort . map swap

unionRel :: Ord a => Rel a -> Rel a -> Rel a
unionRel r s = nub $ sort $ r ++ s

symClos :: Ord a => Rel a -> Rel a
symClos r = unionRel r (inverseRel r)

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos r = unionRel r (r @@ r)
