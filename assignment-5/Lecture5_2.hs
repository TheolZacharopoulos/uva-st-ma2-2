  module Main
  
  where 
  
  import Data.List
  import System.Random

  type Row    = Int 
  type Column = Int
  type Position = (Row,Column)
  type Constrnt = [[Position]]
  type Value  = Int
  type Grid   = [[Value]]
  
  positions :: [Position]
  positions = [(r,c) | r <- [1..9], c <- [1..9]]
  
  values :: [Int]
  values    = [1..9] 
  
  blocks :: [[Int]]
  blocks = [[1..3],[4..6],[7..9]]

------------------------------------------Constraint representations--------------------------------------
  -- In order to add a new constraint, add a new Constrnt and add it to the list constrnts.
  -- Also, implement a new share (two positions share this constraint) function for the new constraint.
  -- And finally, add this share function to the list shares.
  -- The share function is required as this drastically improves the efficiency of the overall program.
  --    Drastically here means: to the level of http://homepages.cwi.nl/~jve/courses/15/testing/lab/Lecture5.hs
  rowConstrnt:: Constrnt
  rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
  
  columnConstrnt :: Constrnt
  columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
  
  blockConstrnt :: Constrnt
  blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]

  constrnts :: [Constrnt]
  constrnts = [rowConstrnt, columnConstrnt, blockConstrnt]


  shareRow :: Position -> Position -> Bool
  shareRow (x1,_) (x2,_) = x1 == x2

  shareColumn :: Position -> Position -> Bool
  shareColumn (_,y1) (_,y2) = y1 == y2

  shareBlock :: Position -> Position -> Bool
  shareBlock (r1,c1) (r2,c2) = 
    r2 `elem` blockR1 && c2 `elem` blockC1
      where
        bl x = concat $ filter (elem x) blocks
        blockR1 = bl r1
        blockC1 = bl c1
  
  shares :: [Position -> Position -> Bool]
  shares = [shareRow, shareColumn, shareBlock]
----------------------------------------------------------------------------------------------------------

  showVal :: Value -> String
  showVal 0 = " "
  showVal d = show d

  showRow :: [Value] -> IO()
  showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
   do  putChar '|'         ; putChar ' '
       putStr (showVal a1) ; putChar ' '
       putStr (showVal a2) ; putChar ' '
       putStr (showVal a3) ; putChar ' '
       putChar '|'         ; putChar ' '
       putStr (showVal a4) ; putChar ' '
       putStr (showVal a5) ; putChar ' '
       putStr (showVal a6) ; putChar ' '
       putChar '|'         ; putChar ' '
       putStr (showVal a7) ; putChar ' '
       putStr (showVal a8) ; putChar ' '
       putStr (showVal a9) ; putChar ' '
       putChar '|'         ; putChar '\n'

  showGrid :: Grid -> IO()
  showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
   do putStrLn ("+-------+-------+-------+")
      showRow as; showRow bs; showRow cs
      putStrLn ("+-------+-------+-------+")
      showRow ds; showRow es; showRow fs
      putStrLn ("+-------+-------+-------+")
      showRow gs; showRow hs; showRow is
      putStrLn ("+-------+-------+-------+")

  type Sudoku = Position -> Value

  sud2grid :: Sudoku -> Grid
  sud2grid s = 
    [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 
  
  grid2sud :: Grid -> Sudoku
  grid2sud gr = \ (r,c) -> pos gr (r,c) 
    where 
    pos :: [[a]] -> Position -> a 
    pos gr (r,c) = (gr !! (r-1)) !! (c-1)

  showSudoku :: Sudoku -> IO()
  showSudoku = showGrid . sud2grid

  freeAtPos :: Sudoku -> Position -> Constrnt -> [Value]
  freeAtPos s (r,c) xs = 
      let ys = filter (elem (r,c)) xs 
    in 
      if null ys
      then values
      else foldl1 intersect (map ((values \\) . map s) ys)

  freeAtPosAll :: Sudoku -> Position -> [Value]
  freeAtPosAll s (r,c) = 
    foldl1 intersect (map (freeAtPos s (r,c)) constrnts)

  sharesConstrnt :: Position -> Position -> Bool
  sharesConstrnt pos1 pos2 = 
    any (\s -> s pos1 pos2) shares

  injective :: Eq a => [a] -> Bool
  injective xs = nub xs == xs

  consistent :: Sudoku -> Bool
  consistent s = 
      and $ map injective nonZeroVals
    where 
      vals = (map (\cs -> map s cs) (concat constrnts))  
      nonZeroVals = map (filter (/= 0)) vals
 
  extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
  extend = update

  update :: Eq a => (a -> b) -> (a,b) -> a -> b 
  update f (y,z) x = if x == y then z else f x 

  type Constraint = (Row,Column,[Value])

  type Node = (Sudoku,[Constraint])
 
  showNode :: Node -> IO()
  showNode = showSudoku . fst

  solved  :: Node -> Bool
  solved = null . snd

  extendNode :: Node -> Constraint -> [Node]
  extendNode (s,constraints) (r,c,vs) = 
     [(extend s ((r,c),v),
       sortBy length3rd $ 
           prune (r,c,v) constraints) | v <- vs ]

  length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
  length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

  prune :: (Row,Column,Value) -> [Constraint] -> [Constraint]
  prune _ [] = []
  prune (r,c,v) ((x,y,zs):rest)
    | sharesConstrnt (r,c) (x,y) = (x,y,zs\\[v]) : prune (r,c,v) rest
    | otherwise = (x,y,zs) : prune (r,c,v) rest

  initNode :: Grid -> [Node]
  initNode gr = let s = grid2sud gr in 
                if (not . consistent) s then [] 
                else [(s, constraints s)]

  openPositions :: Sudoku -> [Position]
  openPositions s = [ pos | pos <- positions, s (pos) == 0 ]

  constraints :: Sudoku -> [Constraint] 
  constraints s = sortBy length3rd 
      [(r,c, freeAtPosAll s (r,c)) | 
                         (r,c) <- openPositions s ]

  data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

  exmple1 = T 1 [T 2 [], T 3 []]
  exmple2 = T 0 [exmple1,exmple1,exmple1]

  grow :: (node -> [node]) -> node -> Tree node 
  grow step seed = T seed (map (grow step) (step seed))

  count :: Tree a -> Int 
  count (T _ ts) = 1 + sum (map count ts)

  search :: (node -> [node]) 
         -> (node -> Bool) -> [node] -> [node]
  search children goal [] = []
  search children goal (x:xs) 
    | goal x    = x : search children goal xs
    | otherwise = search children goal ((children x) ++ xs)

  solveNs :: [Node] -> [Node]
  solveNs = search succNode solved 
  
  succNode :: Node -> [Node]
  succNode (s,[]) = []
  succNode (s,p:ps) = extendNode (s,ps) p 

  solveAndShow :: Grid -> IO[()]
  solveAndShow gr = solveShowNs (initNode gr)
  
  solveShowNs :: [Node] -> IO[()]
  solveShowNs = sequence . fmap showNode . solveNs

  example1 :: Grid
  example1 = [[5,3,0,0,7,0,0,0,0],
              [6,0,0,1,9,5,0,0,0],
              [0,9,8,0,0,0,0,6,0],
              [8,0,0,0,6,0,0,0,3],
              [4,0,0,8,0,3,0,0,1],
              [7,0,0,0,2,0,0,0,6],
              [0,6,0,0,0,0,2,8,0],
              [0,0,0,4,1,9,0,0,5],
              [0,0,0,0,8,0,0,7,9]]

  example2 :: Grid
  example2 = [[0,3,0,0,7,0,0,0,0],
              [6,0,0,1,9,5,0,0,0],
              [0,9,8,0,0,0,0,6,0],
              [8,0,0,0,6,0,0,0,3],
              [4,0,0,8,0,3,0,0,1],
              [7,0,0,0,2,0,0,0,6],
              [0,6,0,0,0,0,2,8,0],
              [0,0,0,4,1,9,0,0,5],
              [0,0,0,0,8,0,0,7,9]]

  example3 :: Grid
  example3 = [[1,0,0,0,3,0,5,0,4],
              [0,0,0,0,0,0,0,0,3],
              [0,0,2,0,0,5,0,9,8], 
              [0,0,9,0,0,0,0,3,0],
              [2,0,0,0,0,0,0,0,7],
              [8,0,3,0,9,1,0,6,0],
              [0,5,1,4,7,0,0,0,0],
              [0,0,0,3,0,0,0,0,0],
              [0,4,0,0,0,9,7,0,0]]

  example4 :: Grid
  example4 = [[1,2,3,4,5,6,7,8,9],
              [2,0,0,0,0,0,0,0,0],
              [3,0,0,0,0,0,0,0,0],
              [4,0,0,0,0,0,0,0,0],
              [5,0,0,0,0,0,0,0,0],
              [6,0,0,0,0,0,0,0,0],
              [7,0,0,0,0,0,0,0,0],
              [8,0,0,0,0,0,0,0,0],
              [9,0,0,0,0,0,0,0,0]]

  example5 :: Grid
  example5 = [[1,0,0,0,0,0,0,0,0],
              [0,2,0,0,0,0,0,0,0],
              [0,0,3,0,0,0,0,0,0],
              [0,0,0,4,0,0,0,0,0],
              [0,0,0,0,5,0,0,0,0],
              [0,0,0,0,0,6,0,0,0],
              [0,0,0,0,0,0,7,0,0],
              [0,0,0,0,0,0,0,8,0],
              [0,0,0,0,0,0,0,0,9]]

  emptyN :: Node
  emptyN = (\ _ -> 0,constraints (\ _ -> 0))

  getRandomInt :: Int -> IO Int
  getRandomInt n = getStdRandom (randomR (0,n))

  getRandomItem :: [a] -> IO [a]
  getRandomItem [] = return []
  getRandomItem xs = do n <- getRandomInt maxi
                        return [xs !! n]
                     where maxi = length xs - 1

  randomize :: Eq a => [a] -> IO [a]
  randomize xs = do y <- getRandomItem xs 
                    if null y 
                      then return []
                      else do ys <- randomize (xs\\y)
                              return (head y:ys)

  sameLen :: Constraint -> Constraint -> Bool
  sameLen (_,_,xs) (_,_,ys) = length xs == length ys

  getRandomCnstr :: [Constraint] -> IO [Constraint]
  getRandomCnstr cs = getRandomItem (f cs) 
    where f [] = []
          f (x:xs) = takeWhile (sameLen x) (x:xs)

  rsuccNode :: Node -> IO [Node]
  rsuccNode (s,cs) = do xs <- getRandomCnstr cs
                        if null xs 
                          then return []
                          else return 
                            (extendNode (s,cs\\xs) (head xs))

  rsolveNs :: [Node] -> IO [Node]
  rsolveNs ns = rsearch rsuccNode solved (return ns)

  rsearch :: (node -> IO [node]) 
              -> (node -> Bool) -> IO [node] -> IO [node]
  rsearch succ goal ionodes = 
    do xs <- ionodes 
       if null xs 
         then return []
         else 
           if goal (head xs) 
             then return [head xs]
             else do ys <- rsearch succ goal (succ (head xs))
                     if (not . null) ys 
                        then return [head ys]
                        else if null (tail xs) then return []
                             else 
                               rsearch 
                                 succ goal (return $ tail xs)

  genRandomSudoku :: IO Node
  genRandomSudoku = do [r] <- rsolveNs [emptyN]
                       return r

  randomS = genRandomSudoku >>= showNode

  uniqueSol :: Node -> Bool
  uniqueSol node = singleton (solveNs [node]) where 
    singleton [] = False
    singleton [x] = True
    singleton (x:y:zs) = False

  eraseS :: Sudoku -> Position -> Sudoku
  eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                       | otherwise      = s (x,y)

  eraseN :: Node -> Position -> Node
  eraseN n (r,c) = (s, constraints s) 
    where s = eraseS (fst n) (r,c) 

  minimalize :: Node -> [Position] -> Node
  minimalize n [] = n
  minimalize n ((r,c):rcs) | uniqueSol n' = minimalize n' rcs
                           | otherwise    = minimalize n  rcs
    where n' = eraseN n (r,c)

  filledPositions :: Sudoku -> [Position]
  filledPositions s = [ pos | pos <- positions, s (pos) /= 0 ]

  genProblem :: Node -> IO Node
  genProblem n = do ys <- randomize xs
                    return (minimalize n ys)
     where xs = filledPositions (fst n)

  run = do
    [r] <- rsolveNs [emptyN]
    showNode r
    s <- genProblem r
    showNode s

  main :: IO ()
  main = do
    setStdGen $ mkStdGen 0
    sequence_ $ take 10 $ repeat run
