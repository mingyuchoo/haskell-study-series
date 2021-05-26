# tutorial-01-first-functions

## How to build

```bash
$ mkdir <project>
$ cd <project>
$ cabal init

...


$ cabal install —only-dependencies
$ cabal update
$ cabal configure
$ cabal check
$ cabal build
$ cabal run
$ cabal sdist
$ cabal upload
$ cabal install
```

## How to test

```bash
$ cd ./app
$ doctest ./Main.hs
```
