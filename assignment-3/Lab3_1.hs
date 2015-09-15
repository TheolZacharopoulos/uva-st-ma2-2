module Lab3_1 where
import Lecture3

contradiction :: Form -> Bool
contradiction = not . satisfiable

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f g = all (\ v -> if evl v f then evl v g else True) (allVals (Impl f g))

equiv :: Form -> Form -> Bool
equiv f g = all (\v -> (evl v f) == (evl v g)) (allVals (Equiv f g))