module Lab3_1 where
import Lecture3

-- Preconditions: None
-- Postconditions:
-- Returns True iff:
-- 		There exists no input, such that Form returns true.
contradiction :: Form -> Bool
contradiction = not . satisfiable

-- Preconditions: None
-- Postconditions:
-- Returns True iff:
-- 		For any input Form returns true
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- Preconditions: None
-- Postconditions:
-- Returns True iff:
-- 		For any input for both Forms:
--			(Form1 returns False) or (Form2 returns True)
entails :: Form -> Form -> Bool
entails f g = all (\ v -> if evl v f then evl v g else True) (allVals (Impl f g))

-- Preconditions: None
-- Preconditions: None
-- Postconditions:
-- Returns True iff:
-- 		For any input for both Forms:
--			The truth value of both forms are the same.
equiv :: Form -> Form -> Bool
equiv f g = all (\v -> (evl v f) == (evl v g)) (allVals (Equiv f g))