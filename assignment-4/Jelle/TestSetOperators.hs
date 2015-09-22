module TestSetOperators where

import SetOrd
import SetOperators
import Data.List
import SetIntGenerator

limit :: Int
limit = 1000

testUnionSet :: IO ()
testUnionSet = do
    s1 <- getRandomSetInt 10
    s2 <- getRandomSetInt 10
    print $ show $ s1 `unionSet` s2
