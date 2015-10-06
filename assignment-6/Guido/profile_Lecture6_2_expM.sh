#! /usr/bin/env sh
rm -v Lecture6_2_expM.prof
ghc -prof -auto-all -o Lecture6_2_expM Lecture6_2_expM.hs
rm -v *.hi *.o
./Lecture6_2_expM +RTS -p
