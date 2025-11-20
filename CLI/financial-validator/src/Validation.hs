module Validation
    ( approveTransaction
    , processTransaction
    , rejectTransaction
    , updateAccountBalance
    , validateAccountStatus
    , validateAmount
    , validateBalance
    , validateDailyLimit
    , validateTransaction
    , withValidation
    ) where

import           Types

-- | 거래 검증 파이프라인 (함수 합성의 힘)
validateTransaction :: Account -> Transaction -> ValidationResult
validateTransaction account tx =
  tx `withValidation` [ validateAmount
                      , validateBalance account
                      , validateDailyLimit account
                      , validateAccountStatus account
                      ]

-- | 검증 헬퍼: 여러 검증 함수를 순차적으로 적용
withValidation :: Transaction -> [Transaction -> ValidationResult] -> ValidationResult
withValidation tx validators = foldl chain (Right tx) validators
  where
    chain (Left err) _        = Left err
    chain (Right t) validator = validator t

-- | 금액 검증
validateAmount :: Transaction -> ValidationResult
validateAmount tx@(Transaction _ _ _ amount _ _) | amount <= 0          = Left InvalidAmount
                                                 | amount > USD 1000000 = Left InvalidAmount  -- 단일 거래 한도
                                                 | otherwise            = Right tx

-- | 잔액 검증 (순수 함수)
validateBalance :: Account -> Transaction -> ValidationResult
validateBalance account tx@(Transaction _ _ txType' amount _ _) =
  case txType' of
    Withdrawal -> checkBalance amount
    Transfer _ -> checkBalance amount
    Payment _  -> checkBalance amount
    Deposit    -> Right tx  -- 입금은 잔액 검증 불필요
  where
    checkBalance amt | accBalance account >= amt = Right tx
                     | otherwise                 = Left InsufficientFunds

-- | 일일 한도 검증
validateDailyLimit :: Account -> Transaction -> ValidationResult
validateDailyLimit account tx@(Transaction _ _ txType' amount _ _) =
  case txType' of
    Withdrawal -> checkLimit
    Transfer _ -> checkLimit
    Payment _  -> checkLimit
    Deposit    -> Right tx
  where
    checkLimit | newSpent <= accDailyLimit account = Right tx
               | otherwise                         = Left DailyLimitExceeded
    newSpent = accDailySpent account + amount

-- | 계좌 상태 검증 (실제로는 더 복잡한 규칙)
validateAccountStatus :: Account -> Transaction -> ValidationResult
validateAccountStatus _ tx = Right tx

-- | 거래 승인 (상태 변경)
approveTransaction :: Transaction -> Transaction
approveTransaction tx = tx { txStatus = Approved }

-- | 거래 거부
rejectTransaction :: RejectionReason -> Transaction -> Transaction
rejectTransaction reason tx = tx { txStatus = Rejected reason }

-- | 거래 처리 (검증 + 상태 업데이트)
processTransaction :: Account -> Transaction -> Transaction
processTransaction account tx =
  case validateTransaction account tx of
    Right validTx -> approveTransaction validTx
    Left reason   -> rejectTransaction reason tx

-- | 계좌 잔액 업데이트 (순수 함수 - 새로운 Account 반환)
updateAccountBalance :: Transaction -> Account -> Account
updateAccountBalance (Transaction _ _ txType' amount _ status) account =
  case status of
    Approved ->
      case txType' of
        Withdrawal -> account { accBalance    = accBalance account - amount
                              , accDailySpent = accDailySpent account + amount
                              }
        Transfer _ -> account { accBalance    = accBalance account - amount
                              , accDailySpent = accDailySpent account + amount
                              }
        Payment _  -> account { accBalance    = accBalance account - amount
                              , accDailySpent = accDailySpent account + amount
                              }
        Deposit    -> account { accBalance = accBalance account + amount
                              }
    _ -> account  -- 승인되지 않은 거래는 잔액 변경 없음
