# haskell-stdin

## How to create a project

```bash
stack new <project-name> mingyuchoo/nix-template
```

## How to build

```bash
stack build
# or
stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS"
```

## How to test as watch mode

```bash
stack test --fast --file-watch --watch-all
# or
stack test --coverage --fast --file-watch --watch-all --haddock
# or
ghcid -c="stack ghci test/Spec.hs"
```

## How to run

```bash
stack run
```