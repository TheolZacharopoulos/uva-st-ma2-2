module CnfConvTest

where

import Lecture3
import Lab3_1
import FormulaGenerator
import CnfConv
import Lecture2Test


-- Name range to be used when generating formulas
formulaGeneratorRange :: (Name, Name)
formulaGeneratorRange = (1, 15)

-- Random sub-formulas to be used
randomSubForms :: Int
randomSubForms = 10

-- Generates a random formula
randomFormCase :: IO Form
randomFormCase = randomForm formulaGeneratorRange randomSubForms

unCurriedIsCnfConv :: Form -> Bool
unCurriedIsCnfConv form = isCnf converted && equiv converted form
    where converted = cnf form

testCnf :: IO ()
testCnf =
    testPost unCurriedIsCnfConv (== True) randomFormCase