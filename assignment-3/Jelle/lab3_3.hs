module Lab3_3 where
import Lecture3
import Data.List ((\\))


cnf :: Form -> Form
cnf = (flatten . cnfr . flatten . nnf . arrowfree)


flatten :: Form  -> Form
flatten (Prop x) = Prop x
flatten (Neg f) = Neg (flatten f)
flatten (Cnj fs) = Cnj (map flatten (flattenCnjBody fs))
    where
        flattenCnj :: Form -> [Form]
        flattenCnj (Cnj fs) = flattenCnjBody fs
        flattenCnj f = [f]

        flattenCnjBody :: [Form] -> [Form]
        flattenCnjBody [] = []
        flattenCnjBody (f:fs) = 
            (flattenCnj f) ++ (flattenCnjBody fs)

flatten (Dsj fs) = Dsj (map flatten (flattenDsjBody fs))
    where
        flattenDsj :: Form -> [Form]
        flattenDsj (Dsj fs) = flattenDsjBody fs
        flattenDsj f = [f]

        flattenDsjBody :: [Form] -> [Form]
        flattenDsjBody [] = []
        flattenDsjBody (f:fs) = 
            (flattenDsj f) ++ (flattenDsjBody fs)


cnfr :: Form -> Form
cnfr (Prop x) = Prop x
cnfr (Neg f) = Neg (cnfr f)
cnfr (Cnj fs) = Cnj (map cnfr fs)
cnfr (Dsj fs) = 
    if length cnj == 0 then
        Dsj (map cnfr fs)
    else
        Cnj (map cnfr (mergeCnj (cnj !! 0) others))
    where 
        findCnj :: [Form] -> [Form] -- TODO: refactor to maybe
        findCnj [] = []
        findCnj ((Cnj fs) : _) = [Cnj fs]
        findCnj (f:fs) = findCnj fs

        cnj = findCnj fs
        others = fs \\ cnj


        mergeCnj :: Form -> [Form] -> [Form]
        mergeCnj _ [] = []
        mergeCnj c@(Cnj fs1) (f:fs2) = applyDistrProp f fs1 ++ (mergeCnj c fs2)

        applyDistrProp :: Form -> [Form] -> [Form]
        applyDistrProp _ [] = []
        applyDistrProp f1 (f2:fs) = 
            [Dsj [f1, f2]] ++ (applyDistrProp f1 fs) 
