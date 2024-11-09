# README

## Basic Stack Commands

```bash
$ stack new {project-name}
# or
$ stack new {project-name} quanterall/basic

$ stack build --test --file-watch --watch-all
# or
# build more faster
$ stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS"

$ stack test --file-watch --watch-all
# or
# test automatically
$ ghcid --command "stack ghci test/Spec.hs"

# https://docs.haskellstack.org/en/stable/build_command/
$ stack test --coverage --fast --file-watch --watch-all --haddock

$ stack run
```

## Add hackages to project's package config file

```bash
vim package.yaml
```

```yaml
...

dependencies:
- base >= 4.18 && < 5
- hspec
- QuickCheck
```

## Add test code

Please check test/*.hs files

## Run test

```bash
$ stack test --file-watch --watch-all
```
