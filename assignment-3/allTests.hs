module AllTests where

import TestParse (testParseCase)
import Lab3_1_tests

testAll :: IO ()
testAll = sequence_ [testParseCase, testAllLab3_1]

adInfinitum :: IO () -> IO ()
adInfinitum = sequence_ . repeat
