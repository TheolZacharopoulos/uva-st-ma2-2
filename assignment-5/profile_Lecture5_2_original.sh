#! /usr/bin/env sh
rm -v Lecture5_2_original.prof
ghc -prof -auto-all -o Lecture5_2_original Lecture5_2_original.hs
rm -v *.hi *.o
./Lecture5_2_original +RTS -p
