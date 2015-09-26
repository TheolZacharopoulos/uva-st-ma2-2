module SymClos where

import Data.List

type Rel a = [(a,a)]

unionRel :: Ord a => Rel a -> Rel a -> Rel a
unionRel r1 r2 = (nub.sort) r1 ++ r2

invRel :: Ord a => Rel a -> Rel a
invRel = (sort.map (\(x,y) -> (y,x))) 

symClos :: Ord a => Rel a -> Rel a
symClos r = sort $ union r $ invRel r
