module CSI (main) where

data Boy = Matthew | Peter | Jack | Arnold | Carl 
    deriving (Eq,Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x

count :: (Eq a) => a -> [a] -> Int
count element list = length (filter (\x -> x == element) list) 

-- True if first boy accuses the second, False otherwise
says :: Boy -> Boy -> Bool

says Matthew Peter = True
says Matthew Jack = True
says Matthew Arnold = True

says Peter Matthew = True
says Peter Jack = True

says Jack x = not ((says Matthew x) || (says Peter x))

says Arnold x = (says Matthew x) `xor` (says Peter x)

says Carl x = not (says Arnold x)

says _ _ = False

-- Gives a list of accusers of the boy
accusers :: Boy -> [Boy]
accusers boy = filter ((flip says) boy) boys

-- Find list of guilty boys
guilty :: [Boy]
guilty = filter ((== 3) . length . accusers) boys

-- Find list of honest boys
honest :: [Boy]
honest = filter (\boy -> count boy correctAccusations == lengthGuilty) boys
  where correctAccusations = concatMap accusers guilty
        lengthGuilty       = length guilty

main :: IO ()
main = do
    putStrLn $ "Honest: " ++ show honest
    putStrLn $ "Guilty: " ++ show guilty
