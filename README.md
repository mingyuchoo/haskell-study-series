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

