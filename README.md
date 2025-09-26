<p align="center">
  <a href="https://github.com/mingyuchoo/haskell-study-series/blob/main/LICENSE"><img alt="license" src="https://img.shields.io/github/license/mingyuchoo/haskell-study-series"/></a>
  <a href="https://github.com/mingyuchoo/haskell-setup-series/issues"><img alt="Issues" src="https://img.shields.io/github/issues/mingyuchoo/haskell-setup-series?color=appveyor" /></a>
  <a href="https://github.com/mingyuchoo/haskell-setup-series/pulls"><img alt="GitHub pull requests" src="https://img.shields.io/github/issues-pr/mingyuchoo/haskell-setup-series?color=appveyor" /></a>
</p>

# README

## How to make dev. environment

```
$ nix develop
```

## Basic Cabal Commands

```bash
$ mkdir {project-name}
$ cd {project-name}
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

## Basic Stack Commands

```bash
$ stack new {project-name}
# or
$ stack new {project-name} quanterall/basic

$ stack build --test --file-watch --watch-all
# or
# build more faster
$ stack build --fast -j4 --ghc-options "-j16 +RTS -A256m -RTS"

$ stack test --file-watch --watch-all
# or
# test automatically
$ ghcid --command "stack ghci test/Spec.hs"

# https://docs.haskellstack.org/en/stable/build_command/
$ stack test --coverage --fast --file-watch --watch-all --haddock

$ stack run
````

## Install Haskell applications using Stack

```bash
stack install \
  ghcid       \
  hindent     \
  hlint       \
  hoogle      \
  ihaskell    \
  ormolu      \
  stylish-haskell
```
## Reformat using Stylish-haskell

```bash
$ stylish-haskell -ri **/*.hs
```
