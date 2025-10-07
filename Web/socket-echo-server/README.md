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
echo "Hello, Haskell!" | nc localhost 8000
You said: Hello, Haskell!

# or

telnet localhost 8000
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Hello, Haskell!
You said: Hello, Haskell!
Connection closed by foreign host
```

## Notes

- "Client say:" 메시지를 "Client says:"로 문법에 맞게 수정했습니다.
- 테스트 설명 문구를 더 자연스러운 영어로 다듬었습니다 (예: "when using ...", "should succeed").
- 테스트 `someFunc`는 실제로 포트 `8000`을 바인딩합니다. 동일 포트를 사용하는 프로세스가 있을 경우 테스트가 `Address already in use`로 실패할 수 있습니다. 테스트 실행 전 서버가 실행 중이지 않은지 확인하거나, 테스트에서는 실제 포트 바인딩을 피하도록 변경하는 것을 권장합니다.
