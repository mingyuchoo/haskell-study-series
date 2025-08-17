# socket-echo-server

## How to create a project

```bash
stack new <project-name> mingyuchoo/new-template
```

## How to build

```bash
stack build
# or
stack build --fast -j4 --ghc-options "-j16 +RTS -A256m -RTS"
```

## How to unit test as watch mode

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


## How to test functionality

```bash
echo "Hello, Haskell!" | nc localhost 4000
You said: Hello, Haskell!

# or

telnet localhost 4000
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Hello, Haskell!
You said: Hello, Haskell!
Connection closed by foreign host
```
