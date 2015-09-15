module CnfConv (cnf, isCnf) where

import Lecture3
import Data.List ((\\))

tautologyForm = (Dsj [Prop 1, (Neg $ Prop 1)])
contradictionForm = (Cnj [Prop 1, (Neg $ Prop 1)])

-- Converts any Form into Conjuctive Normal Form
-- Sidenote: the finaly application of flatten only serves to make the output
-- more understandable (prettier).
cnf :: Form -> Form
cnf = until (\f -> (flatten.cnfr) f == f) (flatten.cnfr)
    . flatten . nnf . filterIllegalForm . arrowfree

-- Check that a formula is in CNF form
isCnf :: Form -> Bool
isCnf (Cnj ps) = all isCnfClause ps
isCnf p        = isCnfClause p

isCnfClause :: Form -> Bool
isCnfClause (Dsj ps) = all isCnfLit ps
isCnfClause p        = isCnfLit p

isCnfLit :: Form -> Bool
isCnfLit (Prop _)       = True
isCnfLit (Neg (Prop _)) = True
isCnfLit _              = False


-- Takes an arrowfree Form.
-- Returns a Form without singular or empty (disjunctions or conjunctions).
filterIllegalForm :: Form -> Form
filterIllegalForm (Prop x) = Prop x
filterIllegalForm (Neg f) = Neg (filterIllegalForm f)
filterIllegalForm (Cnj []) = tautologyForm
filterIllegalForm (Cnj [f]) = filterIllegalForm f
filterIllegalForm (Cnj fs) = Cnj $ map filterIllegalForm fs
filterIllegalForm (Dsj []) = contradictionForm
filterIllegalForm (Dsj [f]) = filterIllegalForm f
filterIllegalForm (Dsj fs) = Dsj $ map filterIllegalForm fs


-- Flattens any arrowfree Form
-- Merges nested conjunctions and disjunctions in one disjunction or
-- conjunction respectively.
flatten :: Form  -> Form
flatten (Prop x) = Prop x
flatten (Neg f) = Neg (flatten f)
flatten (Cnj fs) = Cnj (map flatten (flattenCnjBody fs))
    where
        -- If input is a conjunction, return its body.
        -- Otherwise, return input in a list.
        flattenCnj :: Form -> [Form]
        flattenCnj (Cnj fs) = flattenCnjBody fs
        flattenCnj f = [f]

        -- Takes the body of a conjunction.
        -- Returns an equivalent flattened body.
        flattenCnjBody :: [Form] -> [Form]
        flattenCnjBody = concatMap flattenCnj

flatten (Dsj fs) = Dsj (map flatten (flattenDsjBody fs))
    where
        -- If input is a disjunction, return its body.
        -- Otherwise, return input in a list.
        flattenDsj :: Form -> [Form]
        flattenDsj (Dsj fs) = flattenDsjBody fs
        flattenDsj f = [f]

        -- Takes the body of a disjunction, returns an equivalent flattened body/
        flattenDsjBody :: [Form] -> [Form]
        flattenDsjBody = concatMap flattenDsj

-- Takes a flattened, arrowfree, negation normal form, and legal Form.
-- Returns an equivalent form in conjunction normal form.
-- Note! The result is not flattened.
cnfr :: Form -> Form
cnfr (Prop x) = Prop x
cnfr (Neg f) = Neg (cnfr f)
cnfr (Cnj fs) = Cnj (map cnfr fs)
cnfr (Dsj fs) = 
    if length cnj == 0 then
        Dsj (map cnfr fs)
    else
        (cnfr . flatten) (mergeCnj (cnj !! 0) others)
    where 
        -- Takes a list of forms, returns the first occurance of a conjunction in a list.
        -- If the list of forms has no conjunction, returns empty list.
        findCnj :: [Form] -> [Form] -- TODO: refactor to Maybe Form
        findCnj [] = []
        findCnj ((Cnj fs) : _) = [Cnj fs]
        findCnj (f:fs) = findCnj fs

        cnj = findCnj fs
        others = fs \\ cnj

        -- Takes a Conjunction and a list of Forms, 
        -- Applies the distributive property to every form in the list with the conjunction,
        -- Returns the result in a list.
        mergeCnj :: Form -> [Form] -> Form
        mergeCnj c [] = c
        mergeCnj c@(Cnj fs1) (f:fs2) = (mergeCnj newC fs2)
            where 
                newCBody = applyDistrProp f fs1
                newC = Cnj newCBody

        -- Takes a form, and a list of forms which represents the body of a conjunction.
        -- Applies the distributive property.
        applyDistrProp :: Form -> [Form] -> [Form]
        applyDistrProp _ [] = []
        applyDistrProp f1 (f2:fs) = 
            [Dsj [f1, f2]] ++ (applyDistrProp f1 fs) 
