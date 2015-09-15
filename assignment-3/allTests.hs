module AllTests where

import TestParse (testParseCase)
import Lab3_1_tests
import TestBonus
import CnfConvTest

testAll :: IO ()
testAll = sequence_ [testParseCase, testCnf, testAllLab3_1]

adInfinitum :: IO () -> IO ()
adInfinitum = sequence_ . repeat
