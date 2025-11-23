# demo-Free-Monad-db

## How to create a project

```bash
stack new <project-name> mingyuchoo/new-template
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
ghcid --command "stack ghci test/Spec.hs"
```

## How to run

```bash
stack run
```

You can also use `Makefile` for these works.

## 아키텍처 패턴 설명

### Free Monadl

프로그램을 **데이터(AST, 추상 구문 트리)**로 만듭니다.
비즈니스 로직은 "나는 이것을 하고 싶다"라는 명세서(데이터)를 작성하는 것이고,
실행은 그 명세서를 받아 실제로 수행하는 별도의 인터프리터가 담당합니다.

#### 특징

- 장점: 로직(순수 데이터)과 해석(Interpreter)이 완벽하게 분리됩니다. 프로그램의 흐름을 데이터로 가지고 있으므로 실행 전에 검사하거나 변경하기 쉽습니다.
- 단점: 실행 시 트리를 순회해야 하므로 런타임 오버헤드가 있습니다. 보일러플레이트 코드가 다소 발생합니다.
- 의존성: `package.yaml` 파일에 추가해야할 의존성: `free`
