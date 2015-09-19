module Lab3_1_tests where

import Lecture3
import Lab3_1

v = Prop 1
s = Prop 2

--------------------
-- Cases
--------------------
satisfableCase :: Form
satisfableCase = v

notSatisfableCase :: Form
notSatisfableCase = Cnj [Neg v, v]

tautologyCase :: Form
tautologyCase = Dsj [Neg v, v]

notTautologyCase :: Form
notTautologyCase = Cnj[v, s]

--------------------
-- Tests
--------------------
testSatisfable :: Bool
testSatisfable =
    satisfable satisfableCase

testNotSatisfable :: Bool
testNotSatisfable =
    not $ satisfable notSatisfableCase

testContradiction :: Bool
testContradiction =
    not $ contradiction satisfableCase

testNotContradiction :: Bool
testNotContradiction =
    contradiction notSatisfableCase

testTautology :: Bool
testTautology =
    tautology tautologyCase

testNotTautology :: Bool
testNotTautology =
    not $ tautology notTautologyCase