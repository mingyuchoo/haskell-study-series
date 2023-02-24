# waiting-subway

## Basic Stack Commands

```bash
$ stack new <project-name>
# or
$ stack new <project-name> quanterall/basic

$ stack build
## build more faster
$ stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS"

$ stack test
$ stack run
```