module Lab3_1 where

import Lecture3

satisfable :: Form -> Bool
satisfable f = any (\v -> evl v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = (not.satisfable) f

tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f1 f2 = (\ v -> if evl v f1 then evl v f2 else True) (allVals (Impl f1 f2))

equiv :: Form -> Form -> Bool
equiv f1 f2 = all (\v (evl v f1) == (evl v f2)) (allVals (Equiv f1 f2))