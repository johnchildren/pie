[![Build Status](https://travis-ci.org/johnchildren/pie.svg?branch=master)](https://travis-ci.org/johnchildren/pie)

# pie

Just an interpreter for the pie language described in "The Little Typer" I'm working on as a hobby.

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

Running the tests requires the [`tasty-discover`](https://git.coop/decentral1se/tasty-discover) binary to be installed. This can either be done independently or a nix shell file is provided with the required tools.

Once this condition is met you can run the tests with `cabal new-test`.

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
