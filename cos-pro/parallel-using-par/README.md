# parallel-using-par

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

## How to run

```bash

stack run

```
