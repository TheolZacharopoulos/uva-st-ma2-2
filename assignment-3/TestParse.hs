module TestParse where

import System.Random
import Data.Char
import Data.List
import Lecture3
import Lab3_1
import Lecture2Test
import FormulaGenerator

-- Test method: stringify random formulas and check that their parsed
-- versions are equivalent to their original versions.

-- Outcome: so far, each time we run a test with a 1000 random cases, all tests
-- pass.

-- Preconditions: We do not have any preconditions

-- Postconditions:
-- Returns true iff:
--      Originally generated formula and its stringified and then parsed version be equivalent


-- Name range to be used when generating formulas
formulaGeneratorRange :: (Name, Name)
formulaGeneratorRange = (1, 15)

-- Random sub-formulas to be used
randomSubForms :: Int
randomSubForms = 10

-- Helper function for uncurrying the ``parse`` function
-- Uses an input formula. After that the formula is being stringified with ``show``
-- Next step is the stringified version to be parsed.
-- Finally we compare the original formula and the parsed one using equiv
unCurriedParse :: Form -> Bool
unCurriedParse expectedForm = equiv expectedForm actual
    where actual = (head(parse (show expectedForm)))

-- Generates a random formula
randomFormCase :: IO Form
randomFormCase = randomForm formulaGeneratorRange randomSubForms

-- Initiates a set of parse tests using randomly generated formulas
testParseCase :: IO ()
testParseCase =
    testPost unCurriedParse (== True) randomFormCase
