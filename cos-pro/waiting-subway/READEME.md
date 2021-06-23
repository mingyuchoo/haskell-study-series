# waiting-subway

## Prerequisite

```bash
$ cabal install doctest
$ cabal install hlint
$ cabal install stylish-haskell
$ cabal install apply-refact
```

## How to build

```bash
$ mkdir <project>
$ cd <project>
$ cabal init

...


$ cabal update
$ cabal configure
$ cabal check
$ cabal build
$ cabal run
$ cabal sdist
$ cabal upload
$ cabal install
```

## How to pretty format

```bash
$ stylish-haskell -i ./**/*.hs
```

## How to test

### doctest

```bash
$ cd ./app
$ doctest ./Main.hs
```
