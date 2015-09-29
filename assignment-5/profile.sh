#! /usr/bin/env sh
cp -v Lecture5.prof Lecture5.prof.old
cp -v Lecture5_2.prof Lecture5_2.prof.old

ghc -prof -auto-all -o Lecture5 Lecture5.hs
ghc -prof -auto-all -o Lecture5_2 Lecture5_2.hs
rm -v *.hi *.hs *.o

./Lecture5 +RTS -p
./Lecture5_2 +RTS -p
