module Lecture2Test where

numTests :: Int
numTests = 1000

-- Generalized test framework from lecture 2
testR :: (Show a,  Show b)
      => Int
      -> Int
      -> (a -> b)
      -> (a -> b -> Bool)
      -> IO a
      -> IO ()
testR k n f r gen | k == n    = print (show n ++ " tests passed")
                  | otherwise = do
    h  <- gen
    h' <- return $ f h
    if r h h' then do
        print ("pass (actual input: `" ++ show h ++ "`; actual output: `" ++ show h' ++ "`)")
        testR (k+1) n f r gen
    else error ("fail (actual input: `" ++ show h ++ "`; actual output: `" ++ show h' ++ "`)")

testPost :: (Show a, Show b) => (a -> b) -> (b -> Bool) -> IO a -> IO ()
testPost f p = testR 1 numTests f (\_ -> p)

testRel :: (Show a, Show b) => (a-> b) -> (a -> b -> Bool) -> IO a -> IO ()
testRel = testR 1 numTests
