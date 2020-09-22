# Pocket cube solver

haskell project for "logical programming" class - solver for 2x2x2 rubik's cube

## How to run

It is recommended to compile code with `ghc -O2 cube.hs`. Then simply run `./cube`.

## Input and output

Enter cube as a cube map in the following format with each color being an unique char:

```
UU
UU
FFRRBBLL
FFRRBBLL
DD
DD
```

Example input:
```
gw
or
ybyobwrg
wrbrgobg
ow
yy
```
Example output:
```
R F U' R' F R F R' F U' F'
```

The computation can take up to 1 minute, however most of the cases take only a few seconds.

Output is a sequence of moves using traditional [Rubik's cube notation](https://ruwix.com/the-rubiks-cube/notation/).
