# auto-cuss
this program uses only the base Haskell library.  
to build:
```shell
ghc stat.hs
```
test it with the given test files:
```shell
./stat set
./stat large-set
./stat random-set
```
the program takes a file as an argument with the following format (numbers need not be sorted):
```
title of the data
99
4
100
56
74
17
4
20
83
```
