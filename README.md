<p align="center">
  <a href="https://github.com/mingyuchoo/haskell-study-series/blob/main/LICENSE"><img alt="license" src="https://img.shields.io/github/license/mingyuchoo/haskell-study-series"/></a>
  <a href="https://github.com/mingyuchoo/haskell-setup-series/issues"><img alt="Issues" src="https://img.shields.io/github/issues/mingyuchoo/haskell-setup-series?color=appveyor" /></a>
  <a href="https://github.com/mingyuchoo/haskell-setup-series/pulls"><img alt="GitHub pull requests" src="https://img.shields.io/github/issues-pr/mingyuchoo/haskell-setup-series?color=appveyor" /></a>
</p>

# haskell-setup-series

# Prerequsite

## For Ubuntu

```
$ sudo apt update
$ sudo apt install -y \
  software-properties-common \
  build-essential \
  net-tools \
  musl-tools \
  traceroute \
  ca-certificates \
  gnupg \
  lsb-release \
  libbz2-dev \
  libffi-dev \
  libgdbm-dev \
  libgmp3-dev \
  libncurses5-dev \
  libnss3-dev \
  libreadline-dev \
  libsqlite3-dev \
  libssl-dev \
  libtinfo-dev \
  zlib1g-dev \
  gconf2 \
  gconf-service \
  libappindicator1

```

## Basic Stack Commands

```bash
$ stack new <project-name>
# or
$ stack new <project-name> quanterall/basic

$ stack build
## build more faster
$ stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS"

$ stack test
# or
# test automatically
$ ghcid -c="stack ghci test/Spec.hs"

$ stack run
```

## Basic Cabal Commands

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

## Reformat using Stylish-haskell

```bash
$ stylish-haskell -ri **/*.hs
```

