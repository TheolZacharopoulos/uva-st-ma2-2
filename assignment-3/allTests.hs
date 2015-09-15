module AllTests where

import TestParse (testParseCase)
import Lab3_1_tests
import TestBonus
import CnfConvTest

testAllLab3_1 :: IO ()
testAllLab3_1 = do
    print ("Testing contradiction, tautology, entails, equivalence... " ++ testResult)
    where
        allTests = all (== True) [testContradiction, testTautology, testTautology, testEntails, testEquiv]
        testResult = if allTests then "Passed!" else "Failed!"

testAll :: IO ()
testAll = sequence_ [testParseCase, TestBonus.testCnf, testAllLab3_1, CnfConvTest.testCnf]

adInfinitum :: IO () -> IO ()
adInfinitum = sequence_ . repeat
