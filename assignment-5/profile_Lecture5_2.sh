#! /usr/bin/env sh
cp -v Lecture5_2.prof Lecture5_2.prof.old
ghc -prof -auto-all -o Lecture5_2 Lecture5_2.hs
rm -v *.hi *.o
./Lecture5_2 +RTS -p
