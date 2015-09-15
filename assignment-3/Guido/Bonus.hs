module Bonus (cnf2cls) where

import Lecture3

type Clause  = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls p@(Prop _) = [cnfcl p]
cnf2cls p@(Neg _)  = [cnfcl p]
cnf2cls p@(Dsj _)  = [cnfcl p]
cnf2cls (Cnj ps)   = map cnfcl ps
cnf2cls _          = error "input not in CNF"

cnfcl :: Form -> Clause
cnfcl p@(Prop _) = [cnflit p]
cnfcl p@(Neg _)  = [cnflit p]
cnfcl (Dsj ps)   = map cnflit ps
cnfcl _          = error "input not in CNF"

cnflit :: Form -> Int
cnflit (Prop x)       = x
cnflit (Neg (Prop x)) = -x
cnflit _              = error "input not in CNF"
