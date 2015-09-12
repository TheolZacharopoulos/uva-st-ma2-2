module AllTests where

import TriangleTests (testAllTriangles)
import IsDerangementTests (testAllDerangement)
import IsPermutationTests (testAllPermutation)
import IbanTests (allIbanTests)

testAll :: IO ()
testAll = sequence_ [testAllTriangles, testAllDerangement, testAllPermutation, allIbanTests]