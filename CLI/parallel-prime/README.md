# README

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
```

## Set up for Parallel

```yaml
---
dependencies:
  - paralel
---
executables:
  parallel-using-par-exe:
    ghc-options:
      - -with-rtsopts=-N4
---
tests:
  parallel-using-par-exe:
    ghc-options:
      - -with-rtsopts=-N4
```
