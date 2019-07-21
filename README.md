[![Build Status](https://travis-ci.org/johnchildren/pie.svg?branch=master)](https://travis-ci.org/johnchildren/pie)

# pie

Just an interpreter for the pie language described in ["The Little Typer"](http://www.thelittletyper.com/) I'm working on as a hobby. Buy the book. Seriously. It's good.

## Building

The project can be built with either cabal or nix, run either

```
cabal new-build
```

or

```
nix build -f release.nix
```

as appropriate. Note that building with nix will also run the test suite.

## Development

The tests can be run with `cabal new-test`.

## Repl

The `pie-repl` build target provides a binary that can be used as a pie repl. The easiest way to use this directly from the project is through `cabal new-run pie-repl`.

```
Welcome to pie! Each line will be evaluated as an expr!
Pie> (claim twin (Pi (Y Universe) (-> Y (Pair Y Y))))
Pie> (define twin (lambda (Y) (lambda (x) (cons x x))))
Pie> ((twin Nat) 3)
(the (Pair Nat Nat) ((twin Nat) 3))
Pie> ((twin Atom) 'foo)
(the (Pair Atom Atom) ((twin Atom) 'foo))
```

## Reference Material

- http://www.thelittletyper.com/
- http://davidchristiansen.dk/tutorials/nbe/
