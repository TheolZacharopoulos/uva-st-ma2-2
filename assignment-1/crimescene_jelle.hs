module Main (main) where

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

xor :: Bool -> Bool -> Bool

xor True x = not x
xor False x = x


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


accusers_rec :: Boy -> [Boy] -> [Boy]
accusers_rec _ [] = []
accusers_rec b (first_boy:other_boys) = (accusers_rec b other_boys) ++ 
                                    (if says first_boy b then [first_boy] else [])

accusers :: Boy -> [Boy]
accusers b = accusers_rec b boys 

guilty_rec :: [Boy] -> [Boy]
guilty_rec [] = []
guilty_rec (first_boy:other_boys) = guilty_rec other_boys ++
                                    if (length $ accusers first_boy) == 3 then [first_boy] 
                                    else [] 

guilty :: [Boy]
guilty = guilty_rec boys


honest_rec :: Boy -> [Boy] -> [Boy]
honest_rec _ [] = []
honest_rec guilty_boy (first_boy:other_boys) = (honest_rec guilty_boy other_boys) ++
                                    if (says first_boy guilty_boy) then [first_boy]
                                    else []

honest :: [Boy]
honest = honest_rec (head guilty) boys


main :: IO ()
main = putStrLn $ show $ honest