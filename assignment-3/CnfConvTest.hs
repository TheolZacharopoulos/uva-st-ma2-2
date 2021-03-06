module CnfConvTest where

import System.Random

import Lecture3
import Lab3_1
import FormulaGenerator
import CnfConv
import Lecture2Test


-- Name range to be used when generating formulas
formulaGeneratorRange :: (Name, Name)
formulaGeneratorRange = (1, 5)

-- Random sub-formulas to be used
randomSubForms :: Int
randomSubForms = 30

-- Generates a random formula
randomFormCase :: IO Form
randomFormCase = randomForm formulaGeneratorRange randomSubForms

unCurriedIsCnfConv :: Form -> Bool
unCurriedIsCnfConv form = isCnf converted && equiv converted form
    where converted = cnf form

-- Checks that a formula converted to CNF is equivalent to the original
testCnf :: IO ()
testCnf =
    testPost unCurriedIsCnfConv id randomFormCase

-- Checks that isCnf is True for known CNF formulas
testIsCnfGood :: IO ()
testIsCnfGood =
    testPost isCnf id randomGoodCnfCase

-- Check that isCnf is False for known non-CNF formulas
testIsCnfBad :: IO ()
testIsCnfBad =
    testPost isCnf not randomBadCnfCase

randomGoodCnfCase :: IO Form
randomGoodCnfCase = do
    i <- randomRIO (0, length knownCnfs - 1)
    return $ knownCnfs !! i

randomBadCnfCase :: IO Form
randomBadCnfCase = do
    goodCnf <- randomGoodCnfCase
    n <- randomRIO (0,1) :: IO Int
    return $ case n of
        0 -> Cnj [Cnj [goodCnf]]
        1 -> Dsj [Cnj [goodCnf]]

knownCnfs :: [Form]
knownCnfs =
    [Prop 1
    ,Neg (Prop 1)
    ,Dsj []
    ,Dsj [Prop 1]
    ,Dsj [Prop 1, Neg (Prop 2)]
    ,Cnj []
    ,Cnj [Prop 1]
    ,Cnj [Prop 1, Neg (Prop 2)]
    ,Cnj [Prop 1, Neg (Prop 2), Dsj []]
    ,Cnj [Prop 1, Neg (Prop 2), Dsj [Prop 3]]
    ,Cnj [Prop 1, Neg (Prop 2), Dsj [Prop 3, Neg (Prop 4)]]
    ]
