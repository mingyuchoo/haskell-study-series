# haskell-setup-series

## Basic Stack Commands

```sh
$ stack new <project-name>
# or
$ stack new <project-name> quanterall/basic

$ stack build
$ stack test
$ stack run
```

## Basic Cabal Commands

```sh
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

## Reformat using Stylish-haskell

```sh
$ stylish-haskell -ri **/*.hs
```

