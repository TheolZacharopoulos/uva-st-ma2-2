module Lecture2Test where

-- Generalized test framework from lecture 2
testR :: Show a
      => Int
      -> Int
      -> (a -> b)
      -> (a -> b -> Bool)
      -> IO a
      -> IO ()
testR k n f r gen | k == n    = print (show n ++ " tests passed")
                  | otherwise = do
    h <- gen
    if r h (f h) then do
        print ("pass on: " ++ show h)
        testR (k+1) n f r gen
    else error ("failed test on: " ++ show h)

testPost :: Show a => (a -> b) -> (b -> Bool) -> IO a -> IO ()
testPost f p = testR 1 100 f (\_ -> p)

testRel :: Show a => (a-> b) -> (a -> b -> Bool) -> IO a -> IO ()
testRel = testR 1 100
