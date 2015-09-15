module Lab3_1_tests where

import Lecture3
import Lab3_1

tautologyForm = (Dsj [p, (Neg p)])
contradictionForm = (Cnj [p, (Neg p)])

-- Tests contradiction, by entering all three cases and checking the output:
--      Satisfiable (one that is not also a Tautology) (<- should return False),
--      Contradiction (<- should return True) and
--      Tautology (<- should return False)
testContradiction :: Bool
testContradiction = 
    not (contradiction p) &&
    not (contradiction tautologyForm) &&
    contradiction contradictionForm

-- Tests tautology, by entering all three cases and checking the output:
--      Satisfiable (one that is not also a Tautology) (<- should return False),
--      Contradiction (<- should return False) and
--      Tautology (<- should return True)
testTautology :: Bool
testTautology =
    not (tautology p) &&
    tautology tautologyForm &&
    not (tautology contradictionForm)

-- Tests entails by entering its entire truth table and checking the output.
-- Note! entails takes in formulas, but we can use tautologies and 
-- contradictions to replicate simple truth values.
testEntails :: Bool
testEntails =
    (entails tautologyForm tautologyForm) &&
    not (entails tautologyForm contradictionForm) &&
    (entails contradictionForm tautologyForm) &&
    (entails contradictionForm contradictionForm)

-- Tests equiv by entering its entire truth table and checking the output.
-- Note! equiv takes in formulas, but we can use tautologies and 
-- contradictions to replicate simple truth values.
testEquiv :: Bool
testEquiv =
    (equiv tautologyForm tautologyForm) &&
    not (equiv tautologyForm contradictionForm) &&
    not (equiv contradictionForm tautologyForm) &&
    (equiv contradictionForm contradictionForm)