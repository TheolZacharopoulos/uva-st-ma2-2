module CNF where

import Lecture3

cnf = (dist.nnf.arrowfree)

-- Distributed property.
dist :: Form -> Form
dist (Prop x) = Prop x
dist (Neg f) = Neg (dist f)
dist (Cnj fs) = Cnj (map dist fs)

-- Pv(Q^R) = (PvQ)^(PvR)
dist (Dsj [p@(Prop _), Cnj [f1, f2]]) = Cnj [Dsj[p, f1], Dsj[p, f2]]

-- (Q^R)vP = (PvQ)^(PvR)
dist (Dsj [Cnj [f1, f2], p@(Prop _)]) = Cnj [Dsj[p, f1], Dsj[p, f2]]

dist (Dsj fs) = Dsj (map dist fs)
