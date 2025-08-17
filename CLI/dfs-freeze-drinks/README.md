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

## Search Algorithm

* DFS(Depth-First Search)
* BFS(Breadth-First Search)

### DFS(Depth-First Search)

can implement with `stack` or `recursive`

* Pre-Order
* In-Order
* Post-Order

### BFS(Breadth-First Search)

can implement with `queue`

* Level-Order

