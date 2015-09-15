module TestBonus where

import System.Random

import Bonus
import Lecture2Test
import Lecture3

testCnf :: IO ()
testCnf = Lecture2Test.testRel cnf2cls verifyCnf randomCnf

randomCnf :: IO Form
randomCnf = do
    i <- randomRIO (0, length goodCnfs - 1)
    return $ fst $ goodCnfs !! i

verifyCnf :: Form -> Clauses -> Bool
verifyCnf f c = all id $ map (\(f',c') -> f /= f' || c == c') goodCnfs

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
