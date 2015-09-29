# Assignment 5

## Test reports
*The test reports have been written as comments in the test files.*

## 5.1
Solver: `Lecture5_1.hs`

```
+-------+-------+-------+
| 4 7 8 | 3 9 2 | 6 1 5 |
| 6 1 9 | 7 5 8 | 3 2 4 |
| 2 3 5 | 4 1 6 | 9 7 8 |
+-------+-------+-------+
| 7 2 6 | 8 3 5 | 1 4 9 |
| 8 9 1 | 6 2 4 | 7 5 3 |
| 3 5 4 | 9 7 1 | 2 8 6 |
+-------+-------+-------+
| 5 6 7 | 2 8 9 | 4 3 1 |
| 9 8 3 | 1 4 7 | 5 6 2 |
| 1 4 2 | 5 6 3 | 8 9 7 |
+-------+-------+-------+
```

**Time spent: 2 hours**

## 5.2

To compare the two versions we used Haskell's built-in `-prof` option
during compilation. The resulting executable will dump its performance
profile to a `*.prof` file.

To compile and profile the original and modified version run the corresponding
`.sh` scripts. The original version is `profile_Lecture5_2_original.sh`, the
modified version `profile_Lecture5_2.sh`. To profile both, run

`./profile_Lecture5_2_original.sh && ./profile_Lecture5_2.sh`

This will generate `Lecture5_2_original.prof` and `Lecture5_2.prof`. On our
systems the timings are about equal, the differences are neglible.

The performance for the modified version was worse before but we added some
(premature?) optimisations that reduce extensibility slightly, but boosts
performance up to the original `Lecture5.hs` standards. We sacrifice
extensibility as we now require an implementation of a share function
alongside the `Constrnt` representation for new constraints, for technical
details see `Lecture5_2.hs`.

**Time spent: 6 hours**

## 5.3

See `testMinimal.hs`. To run it, open it in GHCI and execute `runTests`.

**Time spent: 2 hour**

## 5.4

**Time spent: n hours**

## 5.5

Note, the generator takes some time to minimize the generated puzzle, so it might take some minutes before the first puzzle is generated.

**Time spent: 3 hours**

