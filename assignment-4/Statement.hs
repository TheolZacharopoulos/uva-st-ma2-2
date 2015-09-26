module Statement where

import Data.List
import Data.Char
import Test.QuickCheck

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
    show (V x) = show x
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
    show (Prp x)    = show x
    show (Eq x y)   = show x ++ " == " ++ show y 
    show (Lt x y)   = show x ++ " < " ++ show y
    show (Gt x y)   = show x ++ " > " ++ show y
    show (Ng x)     = "-" ++ show x
    show (Cj xs)    = show xs
    show (Dj xs)    = show xs

data Statement = Ass Var Expr
               | Cond Condition Statement Statement
               | Seq [Statement]
               | While Condition Statement
               deriving (Eq)

instance Show Statement where
    show (Ass v x)      = show v ++ " = " ++ show x ++ "\n"
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
