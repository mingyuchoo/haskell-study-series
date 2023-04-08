# parallel-using-par

## Basic Stack Commands

```bash
$ stack new <project-name>
# or
$ stack new <project-name> quanterall/basic

stack build --test --file-watch --watch-all
## build more faster
$ stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS"

$ stack test --file-watch --watch-all
# or
# test automatically
$ ghcid -c="stack ghci test/Spec.hs"

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
