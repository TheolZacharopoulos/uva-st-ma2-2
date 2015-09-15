module AllTests where

import TestParse (testParseCase)
import Lab3_1_tests

testAll :: IO ()
testAll = sequence_ [testParseCase]

adInfinitum :: IO () -> IO ()
adInfinitum = sequence_ . repeat
