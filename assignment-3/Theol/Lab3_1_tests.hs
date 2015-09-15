module Lab3_1_tests where

import Lecture3
import Lab3_1

trueForm = (Dsj [p, (Neg p)])
falseForm = (Cnj [p, (Neg p)])

testContradiction = 
    not (contradiction p) &&
    not (contradiction trueForm) &&
    contradiction falseForm

