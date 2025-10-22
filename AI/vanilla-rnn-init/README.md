# vanilla-rnn-init

Haskell로 RNN을 구현했습니다! 주요 특징은 다음과 같습니다:

## 구현된 기능

### 1. 기본 RNN 구조

입력-은닉층, 은닉층-은닉층(순환), 은닉층-출력층 가중치
편향(bias) 벡터들


### 2. 행렬/벡터 연산

행렬-벡터 곱셈, 벡터 덧셈/뺄셈
외적(outer product), 전치 행렬
순수 함수형으로 구현


### 3. 활성화 함수

Tanh (은닉층용)
Softmax (출력층용)
각각의 미분 함수


### 4. Forward Propagation

한 타임스텝 처리
시퀀스 전체 처리
은닉 상태 관리


### 5. Backward Propagation (BPTT)

그래디언트 구조 정의
파라미터 업데이트
Cross-Entropy 손실 함수



## 실행 방법

```bash
# GHC로 컴파일
ghc -O2 RNN.hs

# 실행
./RNN
```

또는 GHCi에서:

```
bashghci RNN.hs
> main
```
