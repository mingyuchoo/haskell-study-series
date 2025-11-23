# demo-MTL-db

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

### MTL (ReaderT AppEnv IO)

가장 실용적이고 산업 현장에서(e.g., FP Complete 스타일) 널리 쓰이는 방식입니다.
**의존성 주입(Dependency Injection)**을 위해 AppEnv라는 데이터 구조체에 필요한 설정이나 핸들(Handle)을 담고, ReaderT를 통해 이를 전파합니다.

#### 특징

- 장점: 런타임 성능이 가장 빠릅니다. 코드가 직관적이고 IO를 기반으로 하기에 라이브러리 호환성이 좋습니다.
- 단점: 엄밀한 의미에서 효과(Effect)를 제약하기 어렵습니다(결국 IO를 할 수 있으므로).
- 의존성: `package.yaml` 파일에 추가해야할 의존성: `mtl`
