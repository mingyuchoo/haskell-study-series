{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Settlement
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Types
import Validation

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "=== 금융 거래 검증 시스템 ==="
  putStrLn ""

  -- 현재 시간 가져오기 (IO)
  currentTime <- getCurrentTime

  -- 테스트 계좌 생성
  let testAccount =
        Account
          { accId = AccountId "ACC001",
            accBalance = USD 10000,
            accDailyLimit = USD 5000,
            accDailySpent = USD 1000
          }

  -- 테스트 거래들 생성
  let transactions = createTestTransactions currentTime

  -- 각 거래 처리
  putStrLn "📊 거래 처리 결과:"
  putStrLn "----------------------------------------"
  let processedTxs = map (processTransaction testAccount) transactions
  mapM_ printTransaction processedTxs

  putStrLn ""
  putStrLn "📈 정산 보고서:"
  putStrLn "----------------------------------------"
  let report = generateSettlementReport currentTime processedTxs
  printSettlementReport report

  putStrLn ""
  putStrLn "💰 최종 계좌 잔액:"
  putStrLn "----------------------------------------"
  let finalAccount = foldl (flip updateAccountBalance) testAccount processedTxs
  printAccount finalAccount

  putStrLn ""
  putStrLn "🔒 Haskell의 장점 발휘:"
  putStrLn "  ✓ 타입 안정성: USD와 KRW를 혼동할 수 없음"
  putStrLn "  ✓ 순수 함수: 같은 입력은 항상 같은 출력"
  putStrLn "  ✓ 불변성: 원본 거래 데이터는 절대 변경되지 않음"
  putStrLn "  ✓ 대수적 타입: 불가능한 상태 조합을 컴파일 타임에 방지"
  putStrLn "  ✓ 함수 합성: 복잡한 검증 로직을 간단히 조합"

-- | 테스트 거래 생성
createTestTransactions :: UTCTime -> [Transaction]
createTestTransactions time =
  [ Transaction (TransactionId "TX001") (AccountId "ACC001") Withdrawal (USD 500) time Pending,
    Transaction (TransactionId "TX002") (AccountId "ACC001") (Payment "Amazon") (USD 150) time Pending,
    Transaction (TransactionId "TX003") (AccountId "ACC001") Withdrawal (USD 5000) time Pending, -- 일일 한도 초과
    Transaction (TransactionId "TX004") (AccountId "ACC001") Deposit (USD 2000) time Pending,
    Transaction (TransactionId "TX005") (AccountId "ACC001") (Transfer (AccountId "ACC002")) (USD 300) time Pending,
    Transaction (TransactionId "TX006") (AccountId "ACC001") Withdrawal (USD (-100)) time Pending -- 잘못된 금액
  ]

-- | 거래 출력
printTransaction :: Transaction -> IO ()
printTransaction tx =
  let (TransactionId tid) = txId tx
      (USD amt) = txAmount tx
   in putStrLn $ T.unpack tid ++ ": " ++ show (txType tx) ++ " $" ++ show amt ++ " - " ++ statusToString (txStatus tx)

-- | 상태를 문자열로 변환
statusToString :: TransactionStatus -> String
statusToString status = case status of
  Pending -> "대기 중"
  Approved -> "✅ 승인됨"
  Rejected reason -> "❌ 거부됨: " ++ show reason
  Settled -> "💎 정산 완료"

-- | 정산 보고서 출력
printSettlementReport :: SettlementReport -> IO ()
printSettlementReport report =
  let (USD vol) = totalVolume report
   in do
        putStrLn $ "총 거래: " ++ show (totalTransactions report)
        putStrLn $ "승인된 거래: " ++ show (totalApproved report)
        putStrLn $ "거부된 거래: " ++ show (totalRejected report)
        putStrLn $ "총 거래액: $" ++ show vol
        putStrLn "거부 이유:"
        mapM_
          ( \(reason, count) ->
              putStrLn $ "  - " ++ show reason ++ ": " ++ show count
          )
          (rejectionReasons report)

-- | 계좌 정보 출력
printAccount :: Account -> IO ()
printAccount account =
  let (AccountId aid) = accId account
      (USD balance) = accBalance account
      (USD spent) = accDailySpent account
      (USD limit) = accDailyLimit account
   in do
        putStrLn $ "계좌번호: " ++ T.unpack aid
        putStrLn $ "잔액: $" ++ show balance
        putStrLn $ "오늘 사용액: $" ++ show spent ++ " / $" ++ show limit
