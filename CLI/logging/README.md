# logging

## How to create a project

```bash
stack new {project-name} mingyuchoo/new-template
```

## How to build

```bash
stack build
# or
stack build --fast -j4 --ghc-options "-j16 +RTS -A256m -RTS"
```

## How to test as watch mode

```bash
stack test --fast --file-watch --watch-all
# or
stack test --coverage --fast --file-watch --watch-all --haddock
# or
ghcid --command "stack ghci test/Spec.hs"
```

## How to run

```bash
stack run
```
You can also use `Makefile` for these works.

## How to use `ghcid` or `ghci`

```bash
stack ghci --package safe-exceptions
# or
ghci -package safe-exceptions
```

## References

- <https://typeclasses.com/phrasebook/logging>
