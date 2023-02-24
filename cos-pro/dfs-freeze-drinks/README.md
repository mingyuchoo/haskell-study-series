# freeze-drinks

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

