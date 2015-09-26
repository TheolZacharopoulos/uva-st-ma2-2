# Assignment 4

## Run the tests
Each `*Test.hs` module has a `runTests` function defined that runs all the
tests for the module.

## Test reports
*The test reports have been written as comments in the test files.*

## 4.1
* *TODO*

**Time spent: TODO**

## 4.2
See `QCSetGenerator.hs` and `SetIntGenerator.hs`

**Time spent: 3 hours**

## 4.3
See `SetOps.hs` and `SetOpsTest.hs`

**Time spent: 4 hours**

## 4.4
*(Equivalence Classes and Partitions)
I do not understand the equivalence classes and partitions concept and how they are connected to the topics of the lecture. We haven't see them in the workshop.
* In the **implementing relations as function** chapter, there is this implementation of the symmetric:
```
symR' :: [a] -> Rel' a -> Bool
symR' xs r = and [ not (r x y && not (r y x)) | x <- xs, y <- xs]
```
why is this the implementation and not the same as the relation property definition:
```
symR' :: [a] -> Rel' a -> Bool
symR' xs r = and [ (r x y) && (r y x) | x <- xs, y <- xs]
```
same for the transitive.

**Time spent: 2**

## 4.5 and 4.6
See `Relations.hs`

**Time spent: 1 hour**

## 4.7
See `RelationsTest.hs`

**Time spent: 5 hours**

## 4.8
Let _a_ and _b_ be elements of a set.

Let _R<sup>+</sup>_ denote the transitive closure of the relation _R_.

Let _R<sup>s</sup>_ denote the symmetric closure of the relation _R_.

If _R = {(a,b)}_

then _R<sup>+</sup> = R_

and _R<sup>+s</sup> = R<sup>s</sup> = {(a,b),(b,a)}_

and _R<sup>s+</sup> = {(a,a),(a,b),(b,a),(b,b)}_

and _R<sup>+s</sup> =/= R<sup>s+</sup>_
