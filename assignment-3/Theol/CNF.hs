module CNF where

import Lecture3

cnfConv = (dem.nnf.arrowfree)

dem :: Form -> Form
dem (Prop x) = Prop x
dem (Neg f) = Neg (dem f)
dem (Cnj fs) = Cnj (map dem fs)
dem (Dsj fs) = undefined 
    
