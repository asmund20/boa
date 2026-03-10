# Boa

This repo contains a Boa interpreter. The Boa programming language was designed by Andrzej Filiniski, and this project is based on two weekly assignments from the course [Advanced Programming](https://kurser.ku.dk/course/ndaa09013u), where Andrzej is course responsible at the time of writing. It is a small subset of Python, so all valid Boa programs should behave exactly the same when run with the Boa or the Python interpreter. There are some examples of Boa programs in the examples folder.

This Boa language is not the same as [this one](https://boalang.org/).

## Prerequisites
Cabal must be installed.

## Usage
To run a program named `my_program.boa`, run the following command:
```bash
cabal run exe:boa -- my_program.boa
```

To print the parse tree, run:
```bash
cabal run exe:boa -- -p my_program.boa
```
