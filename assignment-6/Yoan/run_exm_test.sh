#! /usr/bin/env sh
cp ExMTests.prof ExMTests.prof.old
ghc -prof -auto-all -o ExMTests ExMTests.hs
rm *.hi *.o
./ExMTests +RTS -p
