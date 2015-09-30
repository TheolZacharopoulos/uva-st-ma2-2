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
optimisations so it is on par with the original `Lecture5.hs`. For the
record, the performance gain from the optimisations has resulted in a decrease
in run-time by at least 50%. The price for this is an extra cost in the
modifiability/extensibility department as we now require an implementation of a
'share' function alongside the `Constrnt` type as the representation for new
constraints. For technical details see `Lecture5_2.hs`.

**Time spent: 6 hours**

## 5.3

See `testMinimal.hs`. To run it, open it in GHCI and execute `runTests`.

**Time spent: 2 hour**

## 5.4

See `emptyBlocks.hs`. It is based on the refactored code `Lecture5_5.hs`.
The available (blocks to be removed) values are 1 to 5, with the following order:
- 1. Middle
- 2. Top Left
- 3. Top Right
- 4. Bottom Left
- 5. Bottom Right

 The algorithm works as follows:
- It firsts generates a solution, 
- then erases n while blocks (*The number of the blocks to be removed is given*)
- and last minimizes based on the previsous stage.

An example of *Five* blocks removed: 
```
*Main> main
+-------+-------+-------+
| 9 3 6 | 7 8 1 | 4 5 2 |
| 1 5 8 | 2 6 4 | 9 7 3 |
| 2 4 7 | 3 9 5 | 1 6 8 |
+-------+-------+-------+
| 7 6 1 | 9 5 2 | 3 8 4 |
| 5 9 4 | 8 3 6 | 7 2 1 |
| 3 8 2 | 4 1 7 | 6 9 5 |
+-------+-------+-------+
| 8 1 9 | 5 7 3 | 2 4 6 |
| 4 7 3 | 6 2 8 | 5 1 9 |
| 6 2 5 | 1 4 9 | 8 3 7 |
+-------+-------+-------+
+-------+-------+-------+
|       | 7 8   |       |
|       |       |       |
|       | 3 9 5 |       |
+-------+-------+-------+
|   6 1 |       | 3   4 |
|     4 |       |       |
| 3   2 |       | 6   5 |
+-------+-------+-------+
|       |   7   |       |
|       | 6   8 |       |
|       | 1     |       |
+-------+-------+-------+
```

**Time spent: 2 hours**

## 5.5

Note, the generator takes some time to minimize the generated puzzle, so it might take some minutes before the first puzzle is generated.

**Time spent: 3 hours**
