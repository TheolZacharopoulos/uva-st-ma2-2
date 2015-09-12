module AllTests where

import TriangleTests (testAllTriangles)
import IsDerangementTests (testAllDerangement)
import IsPermutationTests (testAllPermutation)
import IbanTests (testAllIbans)

testAll :: IO ()
testAll = sequence_ [testAllTriangles, testAllDerangement, testAllPermutation, testAllIbans]
