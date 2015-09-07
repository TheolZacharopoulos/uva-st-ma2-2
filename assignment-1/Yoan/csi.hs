
data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x

says :: Boy -> Boy -> Bool

says Peter Jack = True
says Peter Matthew = True

says Jack x = not(says Peter x) && not(says Matthew x)

-- Carl says what Arnold is saying is false
says Carl x = not(says Arnold x)

-- Mathew says he wasn't neither do Carl
says Matthew Peter = True
says Matthew Jack = True
says Matthew Arnold = True

says Arnold x = (says Matthew x) `xor` (says Peter x)

-- Default
says x y = False

-- Composes list of accusers for a boy
accusersFinder :: Boy -> [Boy] -> [Boy]
accusersFinder _ [] = []
accusersFinder accusedBoy (current:everybody) = (if not (says current accusedBoy) then [] else [current]) 
					++ (accusersFinder accusedBoy everybody)

accusers :: Boy -> [Boy]
accusers b = accusersFinder b boys

accusersCount :: Boy -> Int
accusersCount b = length (accusers b)

allAccusers = map (accusers) boys

-- Finds who has been accused 3 times
guiltyFinder :: [Boy] -> [Boy]
guiltyFinder [] = []
guiltyFinder (current:everybodyElse) = (if accusersCount current == 3 then [current] else [])
					++ (guiltyFinder everybodyElse)

guilty :: [Boy]
guilty = guiltyFinder boys

findHonest :: [Boy] -> [Boy]
findHonest (k:ks) = (if accusersCount k == 3 then (flip says) (accusers k) else findHonest ks)

honest :: [Boy]
honest = findHonest boys

