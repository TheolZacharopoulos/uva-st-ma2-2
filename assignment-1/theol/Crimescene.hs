module Crimescene (main) where

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- The xor logical operation.
xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x

-- True if first boy accuses the second, False otherwise
says :: Boy -> Boy -> Bool

-- Matthew: Carl didn’t do it, and neither did I.
says Matthew Peter = True
says Matthew Jack = True
says Matthew Arnold = True

-- Peter: It was Matthew or it was Jack.
says Peter Matthew = True
says Peter Jack = True

-- Jack: Matthew and Peter are both lying.
says Jack x = not ((says Matthew x) || (says Peter x))

-- Arnold: Matthew or Peter is speaking the truth, but not both.
says Arnold x = (says Matthew x) `xor` (says Peter x)

-- Carl What Arnold says is not true.
says Carl x = not (says Arnold x)

-- The rest
says _ _ = False

-- The list of the accusers of a boy.
accusers :: Boy -> [Boy]
accusers boy = accusersList boy boys
	where 
		accusersList :: Boy -> [Boy] -> [Boy]
		accusersList _ [] = []
		accusersList b (x:xs) = if says x b then x : accusersList b xs else accusersList b xs

-- Gives a list of guilty boys.
guilty :: [Boy]
guilty = filter ((== 3) . length . accusers) boys

count :: (Eq a) => a -> [a] -> Int
count element list = length (filter (\x -> x == element) list) 

-- Gives the list of honest boys, those who accused ALL guilty boys
honest :: [Boy]
honest = map accusers guilty

main :: IO ()
main = do
    putStrLn $ "Honest: " ++ show honest
    putStrLn $ "Guilty: " ++ show guilty