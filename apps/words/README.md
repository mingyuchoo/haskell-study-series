# words

## How to build & run using Nix

### Make Haskell environment

```sh
nix-collect-garbage
nix-shell
```

### run commands

```sh
cabal new-build
cabal run <project-name>-exe
```

## Process

### 1. Developing a Word Game

01. Creating a project with `stack`
02. Printing data structures
03. List Searching
04. Transforming lists with `map`
05. Unit testing with `Hspec`

### 2. Setting up the Word Game Grid

06. Declaring a grid
07. Printing it to the screen
08. Discovering Haskell functions with `Hoogle`
09. `unlines`

### 3. Searching for Words

10. Finding a string in another string with `isInfixOf`
11. `map`
12. `reverse`
13. `Bool` and `Maybe`

### 4. Searching in All Directions

14. `Reverse`
15. `Transpose`
16. Writing a recursive function

### 5. Unit Testing the Grid with HSpec

17. Refactoring data out of the model
18. Testing our functions

## references

-<https://www.srid.ca/haskell-nix>
-<https://nixos.org/guides/nix-pills/garbage-collector.html>
