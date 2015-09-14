module Lab3_3 where
import Lecture3

-- > arrowfree :: Form -> Form 
-- > arrowfree (Prop x) = Prop x 
-- > arrowfree (Neg f) = Neg (arrowfree f)
-- > arrowfree (Cnj fs) = Cnj (map arrowfree fs)
-- > arrowfree (Dsj fs) = Dsj (map arrowfree fs)
-- > arrowfree (Impl f1 f2) = 
-- >   Dsj [Neg (arrowfree f1), arrowfree f2]
-- > arrowfree (Equiv f1 f2) = 
-- >   Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
-- >   where f1' = arrowfree f1
-- >         f2' = arrowfree f2


-- flat nested dsj into one big dsj
-- if there is a cnj in dsj, put it in front of dsj
-- perform distributive property from left to right

cnfr :: Form -> Form
cnfr f | cnf f == f = f
         | otherwise = cnfr f 

cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg f) = Neg (cnf f)
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj [f1, Cnj [f2, f3]]) = 
    Cnj [Dsj [(cnff1), cnf f2], Dsj [cnff1, cnf f3]]
    where 
        cnff1 = cnf f1
cnf (Dsj [Cnj [f1, f2], f3]) = 
    Cnj [Dsj [cnff3, cnf f1], Dsj [cnff3, cnf f2]]
    where 
        cnff3 = cnf f3
cnf (Dsj fs) = Dsj (map cnf fs)
