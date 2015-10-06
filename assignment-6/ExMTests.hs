module ExMTests where

import Lecture6

import Data.Time
import Control.Exception.Base

test b e m = do
   putStrLn $ "Testing expM "++show b++" "++show e++" "++show m
   start <- getCurrentTime
   evaluate $ expM b e m
   stop <- getCurrentTime
   putStrLn ("Finished in " ++ (show $ diffUTCTime stop start))
   putStrLn $ "Testing exM "++show b++" "++show e++" "++show m
   start <- getCurrentTime
   evaluate $ exM b e m
   stop <- getCurrentTime
   putStrLn ("Finished in " ++ (show $ diffUTCTime stop start))

main = sequence_ $ take 3 $ map (\x -> test x x x) (iterate (*10) (10^6))
