module CnfConv where

import Lecture3
import Data.List ((\\))

-- Converts any Form into Conjuctive Normal Form
-- Sidenote: the finaly application of flatten only serves to make the output
-- more understandable (prettier).
cnf :: Form -> Form
cnf = flattenCompress . cnfr . flattenCompress . nnf . flattenCompress
    . filterIllegalForm . arrowfree

-- Check that a formula is in CNF form
isCnf :: Form -> Bool
isCnf (Cnj ps) = all isCnfClause ps
isCnf p        = isCnfClause p

isCnfClause :: Form -> Bool
isCnfClause (Dsj ps) = all isCnfLit ps
isCnfClause p        = isCnfLit p

isCnfLit :: Form -> Bool
isCnfLit (Prop _)       = True
isCnfLit T              = True
isCnfLit F              = True
isCnfLit (Neg (Prop _)) = True
isCnfLit (Neg T)        = True
isCnfLit (Neg F)        = True
isCnfLit _              = False

flattenCompress = until (\f -> (flatten.compress) f == f) (flatten.compress)

-- Compresses a Form
-- Applies some rules of logic when it encounters T's and F's
compress :: Form -> Form
compress = until (\f -> compress' f == f) compress'
    where compress' p@(Prop _)  = p
          compress' (Neg T)     = F
          compress' (Neg F)     = T
          compress' (Neg f)     = Neg $ compress' f
          compress' (Dsj fs)    = if T `elem` fs' then T else Dsj fs'
              where fs' = map compress' fs 
          compress' (Cnj fs)    = if F `elem` fs' then F else Cnj fs'
              where fs' = map compress' fs 
          compress' (Impl F _)  = T
          compress' (Impl _ T)  = T
          compress' (Impl T F)  = F
          compress' (Impl f g)  = Impl (compress' f) (compress' g)
          compress' (Equiv T T) = T
          compress' (Equiv F F) = T
          compress' (Equiv f g) = Equiv (compress' f) (compress' g)
          compress' f           = f


-- Takes an arrowfree Form.
-- Returns a Form without singular or empty (disjunctions or conjunctions).
filterIllegalForm :: Form -> Form
filterIllegalForm (Prop x) = Prop x
filterIllegalForm (Neg f) = Neg (filterIllegalForm f)
filterIllegalForm (Cnj []) = T
filterIllegalForm (Cnj [f]) = filterIllegalForm f
filterIllegalForm (Cnj fs) = Cnj $ map filterIllegalForm fs
filterIllegalForm (Dsj []) = F
filterIllegalForm (Dsj [f]) = filterIllegalForm f
filterIllegalForm (Dsj fs) = Dsj $ map filterIllegalForm fs
filterIllegalForm f = f


-- Flattens any arrowfree Form
-- Merges nested conjunctions and disjunctions in one disjunction or
-- conjunction respectively.
flatten :: Form  -> Form
flatten T = T
flatten F = F
flatten (Prop x) = Prop x
flatten (Neg f) = Neg (flatten f)
flatten (Cnj fs) = Cnj $ foldr flattenBody [] fs
    where
        flattenBody (Cnj fs') rs = foldr flattenBody rs fs'
        flattenBody f'        rs = flatten f' : rs
flatten (Dsj fs) = Dsj $ foldr flattenBody [] fs
    where
        flattenBody (Dsj fs') rs = foldr flattenBody rs fs'
        flattenBody f' rs        = flatten f' : rs

-- Takes a flattened, arrowfree, negation normal form, and legal Form.
-- Returns an equivalent form in conjunction normal form.
-- Note! The result is not flattened.
cnfr :: Form -> Form
cnfr T = T
cnfr F = F
cnfr (Prop x) = Prop x
cnfr (Neg f) = Neg (cnfr f)
cnfr (Cnj fs) = Cnj (map cnfr fs)
cnfr (Dsj fs) = 
    if length cnj == 0 then
        Dsj (map cnfr fs)
    else
        (cnfr . flatten . mergeCnj (head cnj)) others
    where 
        -- Takes a list of forms, returns the first occurance of a conjunction in a list.
        -- If the list of forms has no conjunction, returns empty list.
        -- findCnj :: [Form] -> [Form] -- TODO: refactor to Maybe Form
        -- findCnj [] = []
        -- findCnj ((Cnj fs) : _) = [Cnj fs]
        -- findCnj (f:fs) = findCnj fs

        (cnj, others) = foldr splitOff1Cnj ([],[]) fs
            where splitOff1Cnj f'         ([c], r) = ([c], f':r)
                  splitOff1Cnj f'@(Cnj _) ([], r)  = ([f'], r)
                  splitOff1Cnj f'         (cs, r)  = (cs, f':r)

        -- cnj = findCnj fs
        -- others = fs \\ cnj

        -- Takes a Conjunction and a list of Forms, 
        -- Applies the distributive property to every form in the list with the conjunction,
        -- Returns the result in a list.
        mergeCnj :: Form -> [Form] -> Form
        mergeCnj (Cnj fs1) (f:fs2) = mergeCnj (Cnj $ applyDistrProp f fs1) fs2
        mergeCnj c         _       = c

        -- Takes a form, and a list of forms which represents the body of a conjunction.
        -- Applies the distributive property.
        applyDistrProp :: Form -> [Form] -> [Form]
        applyDistrProp f1 (f2:fs) = Dsj [f1, f2] : applyDistrProp f1 fs
        applyDistrProp _  []      = []
