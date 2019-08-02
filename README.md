[![CircleCI](https://circleci.com/gh/johnchildren/pie.svg?style=svg)](https://circleci.com/gh/johnchildren/pie)

# pie

Just an interpreter for the pie language described in ["The Little Typer"](http://www.thelittletyper.com/) I'm working on as a hobby. Buy the book. Seriously. It's good.

## Building

The project can be built with either cabal, run:

```
cabal new-build
```

## Development

The tests can be run with `cabal new-test`.

## Repl

The `pie-repl` build target provides a binary that can be used as a pie repl. The easiest way to use this directly from the project is through `cabal new-run pie-repl`.

```
Welcome to pie! Each line will be evaluated as an expr!
pie> (claim twin (Pi ((Y Universe)) (-> Y (Pair Y Y))))
claimed
pie> (define twin (lambda (Y) (lambda (x) (cons x x))))
defined
pie> ((twin Nat) 3)
(the (Pair Nat Nat) (cons 3 3))
pie> (which-Nat 0 'foo (lambda (x) 'bar))
(the Atom 'foo)
pie> (which-Nat 1 'bar (lambda (x) 'bar))
(the Atom 'bar)
```

## Reference Material

- http://www.thelittletyper.com/
- http://davidchristiansen.dk/tutorials/nbe/
