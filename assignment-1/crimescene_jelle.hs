module Crimescene (main) where

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]


xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x


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


accusersRec :: Boy -> [Boy] -> [Boy]
accusersRec _ [] = []
accusersRec b (firstBoy:otherBoys) = 
    (accusersRec b otherBoys) ++ (if says firstBoy b then [firstBoy] else [])
-- Gives a list of accusers of the boy                                    
accusers :: Boy -> [Boy]
accusers b = accusersRec b boys 


guiltyRec :: [Boy] -> [Boy]
guiltyRec [] = []
guiltyRec (firstBoy:otherBoys) = 
    guiltyRec otherBoys ++
    if (length $ accusers firstBoy) == 3 then [firstBoy]
    else [] 
-- Gives the list of guilty boy(s)
guilty :: [Boy]
guilty = guiltyRec boys



honestRec :: [Boy] -> [Boy]
honestRec [] = []
honestRec (firstGuilty:otherGuilty) = 
    (honestRec otherGuilty) ++ accusers firstGuilty
-- Gives the list of honest boys, i.e. those who pointed out the guilty boy
honest :: [Boy]
honest = honestRec guilty


main :: IO ()
main = do
    putStrLn $ "Honest: " ++ show honest
    putStrLn $ "Guilty: " ++ show guilty