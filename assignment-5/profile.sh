#! /usr/bin/env sh
ghc -prof -auto-all -o Lecture5 Lecture5.hs
ghc -prof -auto-all -o Lecture5_2 Lecture5_2.hs

./Lecture5 +RTS -p
./Lecture5_2 +RTS -p
