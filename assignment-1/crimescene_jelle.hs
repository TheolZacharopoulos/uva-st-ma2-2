-- Team pt ma2-2

module Crimescene (main) where

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)
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

says x y = False


-- Gives a list of accusers of the boy                                    
accusers :: Boy -> [Boy]
accusers b = accusersRec b boys 
    where
        accusersRec :: Boy -> [Boy] -> [Boy]
        accusersRec _ [] = []
        accusersRec b (firstBoy:otherBoys) = 
            (accusersRec b otherBoys) ++ (if says firstBoy b then [firstBoy] else [])

 
-- Gives the list of guilty boy(s)
guilty :: [Boy]
guilty = guiltyRec boys
    where
        guiltyRec :: [Boy] -> [Boy]
        guiltyRec [] = []
        guiltyRec (firstBoy:otherBoys) = 
            guiltyRec otherBoys ++
            if (length $ accusers firstBoy) == 3 then [firstBoy]
            else []


-- Gives the list of accusers who pointed out a guilty boy
-- Duplicate entries if a boy accused multiple guilty boys
correctAccusers :: [Boy]
correctAccusers = correctAccusersRec guilty
    where
        correctAccusersRec :: [Boy] -> [Boy]
        correctAccusersRec [] = []
        correctAccusersRec (firstGuilty:otherGuilty) = 
            (correctAccusersRec otherGuilty) ++ accusers firstGuilty


-- Gives the list of honest boys, those who accused ALL guilty boys
honest :: [Boy]
honest = 
    filter (\n -> (count n correctAccusers) == (length guilty)) correctAccusers


main :: IO ()
main = do
    putStrLn $ "Honest: " ++ show honest
    putStrLn $ "Guilty: " ++ show guilty