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