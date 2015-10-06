module ExMTests where

import Lecture6

import Data.Time
import Control.Exception.Base

main = do
   putStrLn "Testing with exM function from Lecture 6"
   start <- getCurrentTime
   evaluate (expM (12^6) (12^7) 555)
   stop <- getCurrentTime
   putStrLn ("Finished in " ++ (show $ diffUTCTime stop start))
   putStrLn ("Testing with custom myExM function")
   start <- getCurrentTime
   evaluate (exM (12^6) (12^7) 555)
   stop <- getCurrentTime
   putStrLn ("Finished in " ++ (show $ diffUTCTime stop start))