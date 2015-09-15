module TestParse where

import System.Random
import Data.Char
import Data.List
import Lecture3
import LogicalDefinitions
import Lecture2Test

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

unCurriedParse :: Form -> Bool
unCurriedParse form = equiv form actual
    where actual = (head(parse (show form)))

satisfiableFormCase :: IO Form
satisfiableFormCase = do
    startForm <- genName
    genForm startForm 1

testParseCase :: IO ()
testParseCase =
    testPost unCurriedParse (== True) satisfiableFormCase