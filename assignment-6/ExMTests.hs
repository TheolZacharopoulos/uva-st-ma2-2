module ExMTests where

import Lecture6

import Data.Time
import Data.List
import Control.Exception.Base

average :: Int -> (a -> b) -> a -> IO NominalDiffTime
average n f x = f' 0 (toEnum 0)
    where f' :: Int -> NominalDiffTime -> IO NominalDiffTime
          f' i t | i == n    = return $ t / (toEnum $ n*10^12)
                 | otherwise = do
                    start <- getCurrentTime
                    evaluate $ f x
                    stop <- getCurrentTime
                    f' (i+1) (t + diffUTCTime stop start)

benchmark :: Int -> [(a -> b)] -> [a] -> IO [[NominalDiffTime]]
benchmark n fs = sequence . map (\x -> sequence $ map (\f -> average n f x) fs)

gnuplotBenchmark :: [Integer] -> IO ()
gnuplotBenchmark xs = do
    times <- benchmark 1 [(\e -> expM (10^6) (10^e) 333),(\e -> exM (10^6) (10^e) 333)] xs
    writeDat times
    writeScript 
    where prefix = "exMtests"
          fpd = prefix++(".dat")
          writeDat ds = writeFile fpd $ concat
                                      $ intersperse "\n"
                                      $ fst $ foldr f ([],0) $ reverse ds
            where f times (xs,pos) =
                        (xs ++ [concat (intersperse "\n" slines) ++ "\n"],
                         pos')
                    where slines = map (\(p,t) -> show p ++ "\t" ++ show t)
                                       lines
                          pos'   = ((+1).fst.last) lines
                          lines  = zip (iterate (+0.5) pos) times
          writeScript = writeFile (prefix++".plot") $ concat [
                            "set xtics ("++labels++")\n\n",
                            "set logscale y\n",
                            "set term png size 800,600\nset output\""++prefix++".png\"\n\n",
                            "set boxwidth 0.5\nset style fill solid\n\n",
                            "plot '"++fpd++"' every 2    using 1:2 with boxes ls 1 title \"expM 10^6 10^e 333\",\\\n",
                            "     '"++fpd++"' every 2::1 using 1:2 with boxes ls 2 title \"exM 10^6 10^e 333\""]
            where labels = concat $ intersperse ", "
                                  $ fst $ foldr f ([],0.25) $ reverse xs
                    where f x (ss,pos) = (ss ++ ["\"e="++show x++"\" "++show pos],
                                          pos+1.5)
                        

main = gnuplotBenchmark [0..8]

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

