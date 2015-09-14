> module Lecture3 where
> import Data.List
> import Data.Char

Function Composition, With Flipped Order

With normal order:

(.) :: (a -> b) -> (c -> a) -> (c -> b)
f . g = \ x -> f (g x)

Flip the order:

> infixl 2 #
> 
> (#) :: (a -> b) -> (b -> c) -> (a -> c)
> (#) = flip (.)

Read f # g as f followed by g.

Restatement of the composition rule

From

{ p } f { q }     and    { q } g { r }

conclude:

{ p } f # g { r }

Function Application, with Flipped Order

> infixl 1 $$
> 
> ($$) :: a -> (a -> b) -> b
> ($$) = flip ($)

Example:

*Lecture2> 5 $$ succ
6

Review question: can you work out the types and the definitions of flip
and $?

Review question: why is $ a useful operation?

No Assignment in Pure Functional Programming

Question: what is the difference between   and  ?

Answer: for one thing, the types are different:

  is a function.

  is interpreted in the context of a current memory allocation (an
environment), with   naming a memory cell.

                

Such an environment (for integers, say), is a function of type  , where
is a set of variables.

Assignment as Update

Assignment can be viewed as updating the definition of a function.

> update :: Eq a => (a -> b) -> (a,b) -> a -> b
> update f (x,y) = \ z -> if x == z then y else f z 

> updates :: Eq a => (a -> b) -> [(a,b)] -> a -> b
> updates = foldl update 

The command   can be viewed as an update operation on an environment.

> type Var = String
> type Env = Var -> Integer

To implement variable assignment we need a datatype for expressions, for
the assign command assigns an expression to a variable.

> data Expr = I Integer | V Var 
>           | Add Expr Expr 
>           | Subtr Expr Expr 
>           | Mult Expr Expr 
>           deriving (Eq,Show)

Evaluation of an expression in an environment

> eval :: Env -> Expr -> Integer 
> eval _ (I i) = i 
> eval c (V name) = c name
> eval c (Add e1 e2)   = (eval c e1) + (eval c e2)
> eval c (Subtr e1 e2) = (eval c e1) - (eval c e2)
> eval c (Mult e1 e2)  = (eval c e1) * (eval c e2)

Variable Assignment in an Environment

> assign :: Var -> Expr -> Env -> Env 
> assign var expr c = let 
>    value = eval c expr 
>  in 
>    update c (var,value)

Environment initialisation

An environment is a finite object, so it will yield   (undefined) for all
but a finite number of variables.

The initial environment is everywhere undefined:

> initEnv :: Env 
> initEnv = \ _ -> undefined

Simple example

 initialize an environment; 
 x := 3; 
 y := 5; 
 x := x*y; 
 evaluate x

> example = initEnv $$ 
>           assign "x" (I 3) # 
>           assign "y" (I 5) # 
>           assign "x" (Mult (V "x") (V "y")) #
>           flip eval (V "x")

 *Lecture3> :t example
 example :: Integer
 *Lecture3> example
 15

A closer look at while loops

Joke about the computer scientist who died under the shower...

He read the text on the shampoo bottle and followed the instruction:

lather; rinse; repeat

This is an infinite loop. In many cases we need to add a stop condition:

until clean (lather # rinse)

or

while (not.clean) (lather # rinse)

The until function is predefined in Haskell.

Review question Can you give a type specification of until? Can you give a
definition?

Here is a definition of while:

> while :: (a -> Bool) -> (a -> a) -> a -> a
> while = until . (not.)

Check the types!

Famous example:

> euclid m n = (m,n) $$
>    while (\ (x,y) -> x /= y) 
>          (\ (x,y) -> if x > y then (x-y,y) 
>                               else (x,y-x)) #
>          fst

While + Return

Sometimes it is useful to include a function for transforming the result:

> whiler :: (a -> Bool) -> (a -> a) -> (a -> b) -> a -> b
> whiler p f r = while p f # r

Example:

> euclid2 m n = (m,n) $$
>           whiler (\ (x,y) -> x /= y) 
>                  (\ (x,y) -> if x > y then (x-y,y) 
>                                       else (x,y-x))
>                  fst

Now we can compute the Fibonacci numbers in functional imperative style:

> fibonacci :: Integer -> Integer
> fibonacci n = fibon (0,1,n) where
>   fibon = whiler 
>            (\ (_,_,n) -> n > 0)
>            (\ (x,y,n) -> (y,x+y,n-1))
>            (\ (x,_,_) -> x)

Also compare:

> fb :: Integer -> Integer
> fb n = fb' 0 1 n where 
>    fb' x y 0 = x 
>    fb' x y n = fb' y (x+y) (n-1)

Clearly, these are two versions of the same algorithm.

Really different is the following:

> fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

Review question Try to explain what is going on here.

Back to Hoare Logic

First, let us implement the Hoare triple test. In order to actually run
Hoare tests, we need a domain of test instances. In the previous lecture
we have seen how test instances can be generated. Here we just assume that
we have test instances available.

> hoareTest :: (a -> Bool) -> (a -> a) -> (a -> Bool) -> [a] -> Bool
> hoareTest precondition f postcondition =
>     all (\x -> not (precondition x) || postcondition (f x)) 

An example:

 hoareTest odd succ even [0..100]

Recognizing the Relevant Test Cases

The relevant test cases are the cases that satisfy the precondition. The
following function keeps track of the proportion of relevant tests:

> hoareTestR ::  Fractional t =>
>                (a -> Bool) -> (a -> a) -> (a -> Bool) -> [a] -> (Bool,t)
> hoareTestR precondition f postcondition testcases = let
>        a = fromIntegral (length $ filter precondition testcases)
>        b = fromIntegral (length testcases)
>      in 
>        (all (\x -> not (precondition x) || postcondition (f x))
>        testcases,a/b)

This gives:

*Lecture3>  hoareTest odd succ even [0..100]
True
*Lecture3>  hoareTestR odd succ even [0..100]
(True,0.49504950495049505)
*Lecture3>  hoareTestR (\_ -> True) succ even [0..100]
(False,1.0)
          

The Hoare rule for while statements:

From

{ p } f { p }    

derive

{ p } while c f { p .&&. not.c }

The property p in statement { p } f { p } is called a loop invariant.

> invarTest :: (a -> Bool) -> (a -> a) -> [a] -> Bool
> invarTest invar f = hoareTest invar f invar             

Example use:

    invarTest odd (succ.succ) [0..100]
    

Again, we can implement a variation for keeping track of the proportion of
relevant tests.

> invarTestR ::  Fractional t =>
>                (a -> Bool) -> (a -> a) -> [a] -> (Bool,t)
> invarTestR invar f = hoareTestR invar f invar             

This gives:

*Lecture3> invarTest odd (succ.succ) [0..100]
True
*Lecture3> invarTestR odd (succ.succ) [0..100]
(True,0.49504950495049505)

If you want further instruction on Hoare logic, you can do no better than
consult Mike Gordon's notes.

Note

Hoare triples have a limitation: they do not allow us to express relations
between inputs and outputs of functions.

In many cases, we want to check whether input and output are mapped to the
same image by some test function. For instance, what we would like to say
is that succ.succ preserves parity.

> parity n = mod n 2    

To say this, we need a relational version of Hoare tests:

> testRel :: (a -> a -> Bool) -> (a -> a) -> [a] -> Bool
> testRel spec f = all (\x -> spec x (f x))

Now we can use parity to specify an invariant relation:

> testInvar :: Eq b => (a -> b) -> (a -> a) -> [a] -> Bool
> testInvar specf = testRel (\ x y -> specf x == specf y)

Example use:

 testInvar parity (succ.succ) [0..100]
  

The Logic of Boolean Conditions

The Boolean conditions of any imperative programming language are in fact
statements of propositional logic.

Here is a datatype for propositional formulas:

> type Name = Int

> data Form = Prop Name
>           | Neg  Form
>           | Cnj [Form]
>           | Dsj [Form]
>           | Impl Form Form 
>           | Equiv Form Form 
>           deriving Eq

This time, we define our own show function for formulas:

> instance Show Form where 
>   show (Prop x)   = show x
>   show (Neg f)    = '-' : show f 
>   show (Cnj fs)     = "*(" ++ showLst fs ++ ")"
>   show (Dsj fs)     = "+(" ++ showLst fs ++ ")"
>   show (Impl f1 f2)  = "(" ++ show f1 ++ "==>" 
>                            ++ show f2 ++ ")"
>   show (Equiv f1 f2)  = "(" ++ show f1 ++ "<=>" 
>                            ++ show f2 ++ ")"
> 
> showLst,showRest :: [Form] -> String
> showLst [] = ""
> showLst (f:fs) = show f ++ showRest fs
> showRest [] = ""
> showRest (f:fs) = ' ': show f ++ showRest fs

Example Formulas:

> p = Prop 1
> q = Prop 2
> r = Prop 3 
> 
> form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
> form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
> form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)

Proposition Letters Occurring in a Formula

> propNames :: Form -> [Name]
> propNames = sort.nub.pnames where 
>   pnames (Prop name) = [name]
>   pnames (Neg f)  = pnames f
>   pnames (Cnj fs) = concat (map pnames fs)
>   pnames (Dsj fs) = concat (map pnames fs)
>   pnames (Impl f1 f2)  = concat (map pnames [f1,f2])
>   pnames (Equiv f1 f2) = concat (map pnames [f1,f2])

Valuations

> type Valuation = [(Name,Bool)]
> 
> -- | all possible valuations for lists of prop letters
> genVals :: [Name] -> [Valuation]
> genVals [] = [[]]
> genVals (name:names) = 
>   map ((name,True) :) (genVals names)
>   ++ map ((name,False):) (genVals names)
> 
> -- | generate all possible valuations for a formula
> allVals :: Form -> [Valuation]
> allVals = genVals . propNames

Note that an exponential blowup takes place here. If a propositional
formula has n variables, there are     different valuations for that
formula. To see what that means, just look at this:

 *Lecture3> map (2^) [1..20]
 [2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,
 16384,32768,65536,131072,262144,524288,1048576]

We might as well have defined valuations as environments with boolean
values:

> type ValFct = Name -> Bool

It is all a matter of representation. We can easily move back and forth
between the two representations.

> val2fct :: Valuation -> ValFct
> val2fct = updates (\ _ -> undefined)

> fct2val :: [Name] -> ValFct -> Valuation
> fct2val domain f = map (\x -> (x,f x)) domain 

Evaluation of formulas.

> evl :: Valuation -> Form -> Bool
> evl [] (Prop c)    = error ("no info: " ++ show c)
> evl ((i,b):xs) (Prop c)
>      | c == i    = b
>      | otherwise = evl xs (Prop c)
> evl xs (Neg f)  = not (evl xs f)
> evl xs (Cnj fs) = all (evl xs) fs
> evl xs (Dsj fs) = any (evl xs) fs
> evl xs (Impl f1 f2) = 
>     not (evl xs f1) || evl xs f2
> evl xs (Equiv f1 f2) = evl xs f1 == evl xs f2

Note that the evaluation algorithm is feasible: evl is called only once
for each subformula. Finding a satisfying valuation for a formula is hard,
but checking whether a valuation satisfies a formula is easy.

Satisfiability, Logical Entailment, Equivalence

A formula is satisfiable if some valuation makes it true. We know what the
valuations of a formula f are. These are given by allVals f. We also know
how to express that a valuation v makes a formula f true: eval v f. This
gives:

> satisfiable :: Form -> Bool
> satisfiable f = any (\ v -> evl v f) (allVals f)

Lab Exercise for this week

Write implementations of contradiction, tautology, logical entailment,
logical equivalence, and test them.

Parsing Propositional Formulas

The process of converting an input string to a list of tokens is called
lexical scanning. The following is a relevant list of tokens for
propositional formulas.

> data Token 
>       = TokenNeg
>       | TokenCnj
>       | TokenDsj
>       | TokenImpl
>       | TokenEquiv 
>       | TokenInt Int 
>       | TokenOP
>       | TokenCP
>  deriving (Show,Eq)

The lexer converts a string to a list of tokens.

> lexer :: String -> [Token]
> lexer [] = []
> lexer (c:cs) | isSpace c = lexer cs
>              | isDigit c = lexNum (c:cs) 
> lexer ('(':cs) = TokenOP : lexer cs
> lexer (')':cs) = TokenCP : lexer cs
> lexer ('*':cs) = TokenCnj : lexer cs
> lexer ('+':cs) = TokenDsj : lexer cs
> lexer ('-':cs) = TokenNeg : lexer cs 
> lexer ('=':'=':'>':cs) = TokenImpl : lexer cs
> lexer ('<':'=':'>':cs) = TokenEquiv : lexer cs
> lexer (x:_) = error ("unknown token: " ++ [x])

Read an integer and convert it into a structured token of the form
TokenInt i.

> lexNum cs = TokenInt (read num) : lexer rest
>      where (num,rest) = span isDigit cs

Example use:

 *Lecture3> lexer "*(2 3 -4 +("
 [TokenCnj,TokenOP,TokenInt 2,TokenInt 3,TokenNeg,TokenInt
4,TokenDsj,TokenOP]

A parser for token type a that constructs a datatype b has the following
type:

> type Parser a b = [a] -> [(b,[a])]

The parser constructs a list of tuples (b,[a]) from an initial segment of
a token string [a]. The remainder list in the second element of the result
is the list of tokens that were not used in the construction of the
datatype.

If the output list is empty, the parse has not succeeded. If the output
list more than one element, the token list was ambiguous.

The simplest possible parser is the parser that succeeds immediately,
while consuming no input:

> succeed :: b -> Parser a b
> succeed x xs = [(x,xs)]

Parsing a formula.

> parseForm :: Parser Token Form 
> parseForm (TokenInt x: tokens) = [(Prop x,tokens)]
> parseForm (TokenNeg : tokens) =
>   [ (Neg f, rest) | (f,rest) <- parseForm tokens ]
> parseForm (TokenCnj : TokenOP : tokens) = 
>   [ (Cnj fs, rest) | (fs,rest) <- parseForms tokens ]
> parseForm (TokenDsj : TokenOP : tokens) = 
>   [ (Dsj fs, rest) | (fs,rest) <- parseForms tokens ]
> parseForm (TokenOP : tokens) = 
>   [ (Impl f1 f2, rest) | (f1,ys) <- parseForm tokens,
>                          (f2,rest) <- parseImpl ys ]
>    ++
>   [ (Equiv f1 f2, rest) | (f1,ys) <- parseForm tokens,
>                           (f2,rest) <- parseEquiv ys ] 
> parseForm tokens = []

Parsing a list of formulas: success if a closing parenthesis is
encountered. This uses the succeed parser above.

> parseForms :: Parser Token [Form] 
> parseForms (TokenCP : tokens) = succeed [] tokens
> parseForms tokens = 
>    [(f:fs, rest) | (f,ys) <- parseForm tokens, 
>                    (fs,rest) <- parseForms ys ]

Parsing implications and equivalences uses separate functions, for these
constructions have infix operators.

> parseImpl :: Parser Token Form
> parseImpl (TokenImpl : tokens) = 
>   [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
> parseImpl tokens = []
> 
> parseEquiv :: Parser Token Form
> parseEquiv (TokenEquiv : tokens) = 
>   [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
> parseEquiv tokens = []

The parse function.

> parse :: String -> [Form]
> parse s = [ f | (f,_) <- parseForm (lexer s) ]

This gives:

 *Lecture3> parse "*(1 +(2 -3))"
 [*(1 +(2 -3))]
 *Lecture3> parse "*(1 +(2 -3)"
 []
 *Lecture3> parse "*(1 +(2 -3))))"
 [*(1 +(2 -3))]
 *Lecture3> parseForm (lexer "*(1 +(2 -3))))")
 [(*(1 +(2 -3)),[TokenCP,TokenCP])]

Lab Exercise for this week

Write an appropriate test for the parse function. Hint: use the show
function for formulas.

Conjunctive Normal Form

Conjunctive normal form or CNF and disjunctive normal form or DNF are
important for automated theorem proving: CNF formulas can easily be tested
for validity, by checking that each clause contains some letter   and its
negation  . Automated theorem provers often start out from formulas in
CNF.

First Step

The first step for converting to CNF is to translate into an equivalent
formula that is arrow-free: a formula without   and   operators. Here is
the recipe:

    Use the equivalence between   and   to get rid of   symbols.

    Use the equivalence of   and  , to get rid of   symbols.
(Equivalently: use the equivalence of   and  .)

This conversion has no precondition: it should work for any formula.

> arrowfree :: Form -> Form 
> arrowfree (Prop x) = Prop x 
> arrowfree (Neg f) = Neg (arrowfree f)
> arrowfree (Cnj fs) = Cnj (map arrowfree fs)
> arrowfree (Dsj fs) = Dsj (map arrowfree fs)
> arrowfree (Impl f1 f2) = 
>   Dsj [Neg (arrowfree f1), arrowfree f2]
> arrowfree (Equiv f1 f2) = 
>   Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
>   where f1' = arrowfree f1
>         f2' = arrowfree f2

The postconditions are:

    The result should have no occurrences of Impl and Equiv.

    The result should be logically equivalent to the original.

Second Step

The second step of the translation into CNF is conversion to negation
normal form. Here is the syntactic definition:

 

No  , no   may occur, and negation signs are allowed only in front of
proposition letters. The precondition of the following transformation is
that the input formula is arrowfree. The transformation uses the
equivalences between   and  , between   and  , and between   and  .

> nnf :: Form -> Form 
> nnf (Prop x) = Prop x
> nnf (Neg (Prop x)) = Neg (Prop x)
> nnf (Neg (Neg f)) = nnf f
> nnf (Cnj fs) = Cnj (map nnf fs)
> nnf (Dsj fs) = Dsj (map nnf fs)
> nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
> nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)

Review question What are the pre- and postconditions of nnf?

Review question What are the pre- and postconditions of nnf.arrowfree?

If there is time we will give a short introduction to QuickCheck.

Part of your lab work for this week consists of completing the CNF
conversion process with further steps, and to test the results.

The workshop of today will help you to get more familiar with the language
and semantics of propositional logic.

