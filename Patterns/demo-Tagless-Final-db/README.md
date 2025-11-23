# demo-Tagless-Final-db

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

### Tagless Final

Typeclass를 사용하여 인터페이스(동작)만 정의하고, 구체적인 구현은 나중에 결정하는 방식입니다.
"데이터 구조"를 만들지 않고 함수 합성을 통해 즉시 동작을 정의합니다.

#### 특징

- 장점: 확장이 매우 유연합니다(Expression Problem 해결). 불필요한 데이터 래핑이 없어 컴파일러 최적화(Inlining)가 강력합니다.
- 단점: 타입 시그니처가 복잡해질 수 있고, 에러 메시지가 난해할 수 있습니다.
- 의존성: `package.yaml` 파일에 추가해야할 의존성: 없음
