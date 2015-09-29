#! /usr/bin/env sh
rm -v Lecture5.prof
ghc -prof -auto-all -o Lecture5 Lecture5.hs
rm -v *.hi *.o
./Lecture5 +RTS -p
