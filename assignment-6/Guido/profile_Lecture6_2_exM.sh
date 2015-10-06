#! /usr/bin/env sh
cp -v Lecture6_2_exM.prof Lecture6_2_exM.prof.old
ghc -prof -auto-all -o Lecture6_2_exM Lecture6_2_exM.hs
rm -v *.hi *.o
./Lecture6_2_exM +RTS -p
