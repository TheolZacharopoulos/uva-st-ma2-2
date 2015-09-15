module TestParse where

import System.Random
import Data.Char
import Data.List
import Lecture3
import Lab3_1
import Lecture2Test

-- Preconditions:

-- Postconditions:
-- Returns true if:
--      Originally generated formula and its stringified and then parsed version be equivalent

-- We define a maximum depth limit of formula generation
-- See generateForm function
depthLimit :: Integer
depthLimit = 10

-- Names are generated using a range of numbers.
nameLimit :: Integer
nameLimit = 10

-- Function for name generation
-- Uses nameLimit for range generation
generateName :: IO Form
generateName = do
    name <- randomRIO(1, nameLimit)
    return (Prop name)

-- getForm function resolves and returns formula
-- based on 3 parameters:
--    formUsed - integer value ranging from 1 to 5. Used to choose the right formular
--    a        - first applied formula
--    b        - second applied formula
getForm :: Integer -> Form -> Form -> Form
getForm formUsed a b = case formUsed of
    1 -> Impl a b
    2 -> Equiv a b
    3 -> Neg b
    4 -> Cnj [a, b]
    5 -> Dsj [a, b]

-- Randomly generates new formulas. It is required a starting formula to be provided.
-- Furthermore, a depth counter is required for the recursion.
generateForm :: Form -> Integer -> IO Form
generateForm generated depth = do
    a <- generateName
    b <- generateName
    formType <- randomRIO(1, 5)
    if depth == depthLimit then return generated
    else (generateForm (getForm formType a generated) (depth + 1))

-- Helper function for uncurrying the ``parse`` function
-- Uses an input formula. After that the formula is being stringified with ``show``
-- Next step is the stringified version to be parsed.
-- Finally we compare the original formula and the parsed one using equiv
unCurriedParse :: Form -> Bool
unCurriedParse form = equiv form actual
    where actual = (head(parse (show form)))

-- Generates a random formula. Used as a parameterless wrapper over ``generateForm``
randomFormCase :: IO Form
randomFormCase = do
    startForm <- generateName
    generateForm startForm 1

-- Initiates a set of parse tests using randomly generated formulas
testParseCase :: IO ()
testParseCase =
    testPost unCurriedParse (== True) randomFormCase