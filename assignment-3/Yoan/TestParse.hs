module TestParse where

import System.Random
import Data.Char
import Data.List
import Lecture3
import LogicalDefinitions

depthLimit :: Integer
depthLimit = 10

genName = do
    name <- randomRIO(1, 2)
    return (Prop name)

getForm :: Integer -> Form -> Form -> Form
getForm op a b = case op of
    1 -> Impl a b
    2 -> Equiv a b
    3 -> Neg b
    4 -> Cnj [a, b]
    5 -> Dsj [a, b]

genForm :: Form -> Integer -> IO Form
genForm generated depth = do
    a <- genName
    b <- genName
    formType <- randomRIO(1, 5)
    if depth == depthLimit then return generated
    else (genForm (getForm formType a generated) (depth + 1))

stringify :: IO Form -> IO String
stringify form = do
    form' <- form
    return (show form')

testParse :: IO Form -> IO Bool
testParse form = do
    expected <- form
    stringified <- stringify form
    return (equiv expected (head(parse (show expected))))

satisfiableFormCase :: IO Form
satisfiableFormCase = do
    startForm <- genName
    genForm startForm 1

testSatisfiable :: IO Form -> IO Bool
testSatisfiable form = do
    form' <- form
    return (satisfiable form')

