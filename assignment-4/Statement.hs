module Statement where

import Test.QuickCheck
import Control.Monad
import Data.List
import Data.Char

type Var = String
type Env = Var -> Integer

data Expr = I Integer 
          | V Var 
          | Add Expr Expr 
          | Subtr Expr Expr 
          | Mult Expr Expr 
          deriving (Eq)

instance Show Expr where
    show (I x) = show x
    show (V x) = "$" ++ (filter (/='"') (show x))
    show (Add x y) = show x ++ " + " ++ show y
    show (Subtr x y) = show x ++ " - " ++ show y
    show (Mult x y) = show x ++ " * " ++ show y

data Condition = Prp Var 
               | Eq Expr Expr 
               | Lt Expr Expr 
               | Gt Expr Expr 
               | Ng Condition 
               | Cj [Condition] 
               | Dj [Condition]
               deriving (Eq)

instance Show Condition where
    show (Prp x)    = "$" ++ (filter (/='"') (show x))
    show (Eq x y)   = show x ++ " == " ++ show y 
    show (Lt x y)   = show x ++ " < " ++ show y
    show (Gt x y)   = show x ++ " > " ++ show y
    show (Ng x)     = "!" ++ show x
    show (Cj fs)     = "^(" ++ showCondLst fs ++ ")"
    show (Dj fs)     = "v(" ++ showCondLst fs ++ ")"

showCondLst,showCondRest :: [Condition] -> String
showCondLst [] = ""
showCondLst (f:fs) = show f ++ showCondRest fs
showCondRest [] = ""
showCondRest (f:fs) = ' ': show f ++ showCondRest fs

data Statement = Ass Var Expr
               | Cond Condition Statement Statement
               | Seq [Statement]
               | While Condition Statement
               deriving (Eq)

instance Show Statement where
    show (Ass v x)      = "$" ++ (filter (/='"') (show v)) ++ " = " ++ show x ++ "\n"
    show (Cond c x y)   = "if (" ++ show c ++ ")" ++ " {\n" ++ 
                           showTabbed x ++ "} " ++ 
                          "else {\n" ++ showTabbed y ++ "}"
    show (Seq (sts))    = showSts sts
    show (While c st)   = "while (" ++ show c ++ ") {\n" ++
                          showTabbed st ++ "}"

showTabbed :: Statement -> String
showTabbed (Seq (st:sts)) = "  " ++ show st ++ showTabbed (Seq (sts))
showTabbed (Seq []) = ""
showTabbed st = "  " ++ show st

showSts :: [Statement] -> String
showSts [] = ""
showSts (f:fs) = show f ++ showSts fs

-- Arbitrary Instance for expressions
instance Arbitrary Expr where
    arbitrary = sizedArbitraryExpr

sizedArbitraryExpr :: Gen Expr
sizedArbitraryExpr = sized rndExpr

rndExpr :: Integral a => a -> Gen Expr
rndExpr 0 =
    oneof [
        liftM I (elements [1..100]),
        liftM V (listOf (elements ['a' ..'z']))
    ]

rndExpr n | n > 0 =
    oneof [
        liftM I (elements [1..100]),
        liftM V (listOf (elements ['a' ..'z'])),
        liftM2 Add   subExpr subExpr,
        liftM2 Subtr subExpr subExpr,
        liftM2 Mult  subExpr subExpr
      ]
    where
        subExpr :: Gen Expr
        subExpr = rndExpr (n `div` 2)

-- Arbitrary Instance for conditions
instance Arbitrary Condition where
    arbitrary = sizedArbitraryCondition

sizedArbitraryCondition :: Gen Condition
sizedArbitraryCondition = sized rndCondition

rndCondition :: Integral a => a -> Gen Condition
rndCondition 0 =
    liftM Prp (listOf1 (elements ['a' ..'z']))
rndCondition n | n > 0 =
    frequency [
        (10, liftM Prp (listOf1 (elements ['a' ..'z']))),
        (9, liftM2 Eq arbitrary arbitrary),
        (9, liftM2 Lt arbitrary arbitrary),
        (9, liftM2 Gt arbitrary arbitrary),
        (7, liftM Ng subCondition),
        (1, liftM Cj (listOf subCondition)),
        (1, liftM Dj (listOf subCondition))
      ]
    where
        subCondition :: Gen Condition
        subCondition = rndCondition (n `div` 2)

-- Arbitrary Instance for statements
instance Arbitrary Statement where
    arbitrary = sizedArbitraryStatement

sizedArbitraryStatement :: Gen Statement
sizedArbitraryStatement = sized rndStatement

rndStatement :: Integral a => a -> Gen Statement
rndStatement 0 =
    liftM2 Ass (listOf1 (elements ['a' ..'z'])) arbitrary

rndStatement n | n > 0 =
    frequency [
        (10, liftM2 Ass (listOf (elements ['a' ..'z'])) arbitrary),
        (5,  liftM3 Cond arbitrary subStat subStat),
        (3,  liftM Seq (listOf subStat)),
        (2,  liftM2 While arbitrary subStat)
      ]
    where
        -- Generates a form and divides n to control the size of recursive depth.
        subStat :: Gen Statement
        subStat = rndStatement (n `div` 2)

-----------------------------------
-- Examples
fib :: Statement
fib = Seq [Ass "x" (I 0), Ass "y" (I 1),
           While (Gt (V "n") (I 0))
             (Seq [Ass "z" (V "x"),
                   Ass "x" (V "y"),
                   Ass "y" (Add (V "z") (V "y")),
                   Ass "n" (Subtr (V "n") (I 1))])]

simpleIf :: Statement
simpleIf = Seq [Ass "age" (I 25),
                (Cond (Gt (V "age") (I 20)))
                    (Ass "Access" (I 1))
                    (Ass "Access" (I 0))]

generateStatement :: IO ()
generateStatement = sample (arbitrary::Gen Statement)