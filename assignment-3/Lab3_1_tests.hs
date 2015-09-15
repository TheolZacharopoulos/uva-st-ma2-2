module Lab3_1_tests where
import Lecture3
import Lab3_1

tautologyForm = (Dsj [p, (Neg p)])
contradictionForm = (Cnj [p, (Neg p)])

testContradiction :: Bool
testContradiction = 
    not (contradiction p) &&
    not (contradiction tautologyForm) &&
    contradiction contradictionForm

testTautology :: Bool
testTautology =
    not (tautology p) &&
    tautology tautologyForm &&
    not (tautology contradictionForm)

testEntails :: Bool
testEntails =
    (entails tautologyForm tautologyForm) &&
    not (entails tautologyForm contradictionForm) &&
    (entails contradictionForm tautologyForm) &&
    (entails contradictionForm contradictionForm)

testEquiv :: Bool
testEquiv =
    (equiv tautologyForm tautologyForm) &&
    not (equiv tautologyForm contradictionForm) &&
    not (equiv contradictionForm tautologyForm) &&
    (equiv contradictionForm contradictionForm)

testAllLab3_1 :: IO ()
testAllLab3_1 = do
    print ("Testing contradiction, tautology, entails, equivalence... " ++ testResult)
    where
        allTests = all (== True) [testContradiction, testTautology, testTautology, testEntails, testEquiv]
        testResult = if allTests then "Passed!" else "Failed!"