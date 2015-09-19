module TestBonus where

import System.Random

import Bonus
import Lecture2Test
import Lecture3

-- Preconditions for cnf2cls : Form is in CNF

{- We test the cnf2cls function by giving it a reasonably complete list
 - (in the sense of syntactical variety) of valid CNF formulas.
 -}

testCnf :: IO ()
testCnf = Lecture2Test.testRel cnf2cls verifyCnf randomCnf

-- Get a random good CNF formula
randomCnf :: IO Form
randomCnf = do
    i <- randomRIO (0, length goodCnfs - 1)
    return $ fst $ goodCnfs !! i

-- Verifies that the generated Clauses is correct for the input Form by
-- checking the occurrence in the list of goodCnfs
verifyCnf :: Form -> Clauses -> Bool
verifyCnf f c = all (\(f',c') -> f /= f' || c == c') goodCnfs

-- A list of valid CNF formulas with the expected resulting Clauses format
-- of the formula after cnf2cls is applied.
goodCnfs :: [(Form, Clauses)]
goodCnfs =
    [(Prop 1, [[1]])
    ,(Neg (Prop 1), [[-1]])
    ,(Dsj [], [[]])
    ,(Dsj [Prop 1], [[1]])
    ,(Dsj [Prop 1, Neg (Prop 2)], [[1,-2]])
    ,(Cnj [], [])
    ,(Cnj [Prop 1], [[1]])
    ,(Cnj [Prop 1, Neg (Prop 2)], [[1],[-2]])
    ,(Cnj [Prop 1, Neg (Prop 2), Dsj []], [[1],[-2],[]])
    ,(Cnj [Prop 1, Neg (Prop 2), Dsj [Prop 3]], [[1],[-2],[3]])
    ,(Cnj [Prop 1, Neg (Prop 2), Dsj [Prop 3, Neg (Prop 4)]], [[1],[-2],[3,-4]])
    ]
