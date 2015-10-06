import Data.List
import Data.Time
import Control.Exception.Base
import Lecture6
import ExM

main = do
   putStrLn "Testing with exM function from Lecture 6"
   start <- getCurrentTime
   evaluate (exM (12^6) (12^7) 555)
   stop <- getCurrentTime
   putStrLn ("Finished in " ++ (show $ diffUTCTime stop start))
   putStrLn ("Testing with custom myExM function")
   start <- getCurrentTime
   evaluate (myExM (12^6) (12^7) 555)
   stop <- getCurrentTime
   putStrLn ("Finished in " ++ (show $ diffUTCTime stop start))