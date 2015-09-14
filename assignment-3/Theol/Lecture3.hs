  module Lecture3
  
  where 
  
  import Data.List
  import Data.Char

  infixl 2 #
 
  (#) :: (a -> b) -> (b -> c) -> (a -> c)
  (#) = flip (.)

  infixl 1 $$
  
  ($$) :: a -> (a -> b) -> b
  ($$) = flip ($)

  update :: Eq a => (a -> b) -> (a,b) -> a -> b
  update f (x,y) = \ z -> if x == z then y else f z 

  updates :: Eq a => (a -> b) -> [(a,b)] -> a -> b
  updates = foldl update 

  type Var = String
  type Env = Var -> Integer

  data Expr = I Integer | V Var 
            | Add Expr Expr 
            | Subtr Expr Expr 
            | Mult Expr Expr 
            deriving (Eq,Show)

  eval :: Env -> Expr -> Integer 
  eval _ (I i) = i 
  eval c (V name) = c name
  eval c (Add e1 e2)   = (eval c e1) + (eval c e2)
  eval c (Subtr e1 e2) = (eval c e1) - (eval c e2)
  eval c (Mult e1 e2)  = (eval c e1) * (eval c e2)

  assign :: Var -> Expr -> Env -> Env 
  assign var expr c = let 
     value = eval c expr 
   in 
     update c (var,value)

  initEnv :: Env 
  initEnv = \ _ -> undefined

  example = initEnv $$ 
            assign "x" (I 3) # 
            assign "y" (I 5) # 
            assign "x" (Mult (V "x") (V "y")) #
            flip eval (V "x")

  while :: (a -> Bool) -> (a -> a) -> a -> a
  while = until . (not.)

  euclid m n = (m,n) $$
     while (\ (x,y) -> x /= y) 
           (\ (x,y) -> if x > y then (x-y,y) 
                                else (x,y-x)) #
           fst

  whiler :: (a -> Bool) -> (a -> a) -> (a -> b) -> a -> b
  whiler p f r = while p f # r

  euclid2 m n = (m,n) $$
            whiler (\ (x,y) -> x /= y) 
                   (\ (x,y) -> if x > y then (x-y,y) 
                                        else (x,y-x))
                   fst

  fibonacci :: Integer -> Integer
  fibonacci n = fibon (0,1,n) where
    fibon = whiler 
             (\ (_,_,n) -> n > 0)
             (\ (x,y,n) -> (y,x+y,n-1))
             (\ (x,_,_) -> x)

  fb :: Integer -> Integer
  fb n = fb' 0 1 n where 
     fb' x y 0 = x 
     fb' x y n = fb' y (x+y) (n-1)

  fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

  hoareTest :: (a -> Bool) -> (a -> a) -> (a -> Bool) -> [a] -> Bool
  hoareTest precondition f postcondition =
      all (\x -> not (precondition x) || postcondition (f x)) 

  hoareTestR ::  Fractional t =>
                 (a -> Bool) -> (a -> a) -> (a -> Bool) -> [a] -> (Bool,t)
  hoareTestR precondition f postcondition testcases = let
         a = fromIntegral (length $ filter precondition testcases)
         b = fromIntegral (length testcases)
       in 
         (all (\x -> not (precondition x) || postcondition (f x)) testcases,a/b)

  invarTest :: (a -> Bool) -> (a -> a) -> [a] -> Bool
  invarTest invar f = hoareTest invar f invar             

  invarTestR ::  Fractional t =>
                 (a -> Bool) -> (a -> a) -> [a] -> (Bool,t)
  invarTestR invar f = hoareTestR invar f invar             

  parity n = mod n 2    

  testRel :: (a -> a -> Bool) -> (a -> a) -> [a] -> Bool
  testRel spec f = all (\x -> spec x (f x))

  testInvar :: Eq b => (a -> b) -> (a -> a) -> [a] -> Bool
  testInvar specf = testRel (\ x y -> specf x == specf y)

  type Name = Int

  data Form = Prop Name
            | Neg  Form
            | Cnj [Form]
            | Dsj [Form]
            | Impl Form Form 
            | Equiv Form Form 
            deriving Eq

  instance Show Form where 
    show (Prop x)   = show x
    show (Neg f)    = '-' : show f 
    show (Cnj fs)     = "*(" ++ showLst fs ++ ")"
    show (Dsj fs)     = "+(" ++ showLst fs ++ ")"
    show (Impl f1 f2)  = "(" ++ show f1 ++ "==>" 
                             ++ show f2 ++ ")"
    show (Equiv f1 f2)  = "(" ++ show f1 ++ "<=>" 
                             ++ show f2 ++ ")"
  
  showLst,showRest :: [Form] -> String
  showLst [] = ""
  showLst (f:fs) = show f ++ showRest fs
  showRest [] = ""
  showRest (f:fs) = ' ': show f ++ showRest fs

  p = Prop 1
  q = Prop 2
  r = Prop 3 
  
  form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
  form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
  form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)

  propNames :: Form -> [Name]
  propNames = sort.nub.pnames where 
    pnames (Prop name) = [name]
    pnames (Neg f)  = pnames f
    pnames (Cnj fs) = concat (map pnames fs)
    pnames (Dsj fs) = concat (map pnames fs)
    pnames (Impl f1 f2)  = concat (map pnames [f1,f2])
    pnames (Equiv f1 f2) = concat (map pnames [f1,f2])

  type Valuation = [(Name,Bool)]
  
  -- | all possible valuations for lists of prop letters
  genVals :: [Name] -> [Valuation]
  genVals [] = [[]]
  genVals (name:names) = 
    map ((name,True) :) (genVals names)
    ++ map ((name,False):) (genVals names)
  
  -- | generate all possible valuations for a formula
  allVals :: Form -> [Valuation]
  allVals = genVals . propNames

  type ValFct = Name -> Bool

  val2fct :: Valuation -> ValFct
  val2fct = updates (\ _ -> undefined)

  fct2val :: [Name] -> ValFct -> Valuation
  fct2val domain f = map (\x -> (x,f x)) domain 

  evl :: Valuation -> Form -> Bool
  evl [] (Prop c)    = error ("no info: " ++ show c)
  evl ((i,b):xs) (Prop c)
       | c == i    = b
       | otherwise = evl xs (Prop c)
  evl xs (Neg f)  = not (evl xs f)
  evl xs (Cnj fs) = all (evl xs) fs
  evl xs (Dsj fs) = any (evl xs) fs
  evl xs (Impl f1 f2) = 
      not (evl xs f1) || evl xs f2
  evl xs (Equiv f1 f2) = evl xs f1 == evl xs f2

  satisfiable :: Form -> Bool
  satisfiable f = any (\ v -> evl v f) (allVals f)

  data Token 
        = TokenNeg
        | TokenCnj
        | TokenDsj
        | TokenImpl
        | TokenEquiv 
        | TokenInt Int 
        | TokenOP
        | TokenCP
   deriving (Show,Eq)

  lexer :: String -> [Token]
  lexer [] = []
  lexer (c:cs) | isSpace c = lexer cs
               | isDigit c = lexNum (c:cs) 
  lexer ('(':cs) = TokenOP : lexer cs
  lexer (')':cs) = TokenCP : lexer cs
  lexer ('*':cs) = TokenCnj : lexer cs
  lexer ('+':cs) = TokenDsj : lexer cs
  lexer ('-':cs) = TokenNeg : lexer cs 
  lexer ('=':'=':'>':cs) = TokenImpl : lexer cs
  lexer ('<':'=':'>':cs) = TokenEquiv : lexer cs
  lexer (x:_) = error ("unknown token: " ++ [x])

  lexNum cs = TokenInt (read num) : lexer rest
       where (num,rest) = span isDigit cs

  type Parser a b = [a] -> [(b,[a])]

  succeed :: b -> Parser a b
  succeed x xs = [(x,xs)]

  parseForm :: Parser Token Form 
  parseForm (TokenInt x: tokens) = [(Prop x,tokens)]
  parseForm (TokenNeg : tokens) =
    [ (Neg f, rest) | (f,rest) <- parseForm tokens ]
  parseForm (TokenCnj : TokenOP : tokens) = 
    [ (Cnj fs, rest) | (fs,rest) <- parseForms tokens ]
  parseForm (TokenDsj : TokenOP : tokens) = 
    [ (Dsj fs, rest) | (fs,rest) <- parseForms tokens ]
  parseForm (TokenOP : tokens) = 
    [ (Impl f1 f2, rest) | (f1,ys) <- parseForm tokens,
                           (f2,rest) <- parseImpl ys ]
     ++
    [ (Equiv f1 f2, rest) | (f1,ys) <- parseForm tokens,
                            (f2,rest) <- parseEquiv ys ] 
  parseForm tokens = []

  parseForms :: Parser Token [Form] 
  parseForms (TokenCP : tokens) = succeed [] tokens
  parseForms tokens = 
     [(f:fs, rest) | (f,ys) <- parseForm tokens, 
                     (fs,rest) <- parseForms ys ]

  parseImpl :: Parser Token Form
  parseImpl (TokenImpl : tokens) = 
    [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
  parseImpl tokens = []
  
  parseEquiv :: Parser Token Form
  parseEquiv (TokenEquiv : tokens) = 
    [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
  parseEquiv tokens = []

  parse :: String -> [Form]
  parse s = [ f | (f,_) <- parseForm (lexer s) ]

  arrowfree :: Form -> Form 
  arrowfree (Prop x) = Prop x 
  arrowfree (Neg f) = Neg (arrowfree f)
  arrowfree (Cnj fs) = Cnj (map arrowfree fs)
  arrowfree (Dsj fs) = Dsj (map arrowfree fs)
  arrowfree (Impl f1 f2) = 
    Dsj [Neg (arrowfree f1), arrowfree f2]
  arrowfree (Equiv f1 f2) = 
    Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
    where f1' = arrowfree f1
          f2' = arrowfree f2

  nnf :: Form -> Form 
  nnf (Prop x) = Prop x
  nnf (Neg (Prop x)) = Neg (Prop x)
  nnf (Neg (Neg f)) = nnf f
  nnf (Cnj fs) = Cnj (map nnf fs)
  nnf (Dsj fs) = Dsj (map nnf fs)
  nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
  nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)

