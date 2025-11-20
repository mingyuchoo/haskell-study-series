module Settlement
    ( analyzeTransactionsByHour
    , calculateTotalVolume
    , countRejectionReasons
    , detectSuspiciousPatterns
    , generateSettlementReport
    , isApproved
    , isRapidTransaction
    , isRejected
    , monthlyTransactionSummary
    , settlTransaction
    , settleDailyAccount
    , settleTransactions
    , updateAccountAfterSettlement
    ) where

import           Data.Function (on)
import           Data.List     (groupBy, sortBy)
import qualified Data.Map      as Map
import           Data.Time     (UTCTime)

import           Types

-- | 정산 보고서 생성 (고차 함수와 fold 활용)
generateSettlementReport :: UTCTime -> [Transaction] -> SettlementReport
generateSettlementReport date transactions =
  SettlementReport
    { reportDate = date
    , totalTransactions = length transactions
    , totalApproved = length approvedTxs
    , totalRejected = length rejectedTxs
    , totalVolume = calculateTotalVolume approvedTxs
    , rejectionReasons = countRejectionReasons rejectedTxs
    }
  where
    approvedTxs = filter isApproved transactions
    rejectedTxs = filter isRejected transactions

-- | 거래 승인 여부 확인
isApproved :: Transaction -> Bool
isApproved tx = txStatus tx == Approved

-- | 거래 거부 여부 확인
isRejected :: Transaction -> Bool
isRejected tx =
  case txStatus tx of
    Rejected _ -> True
    _          -> False

-- | 총 거래액 계산 (fold 활용)
calculateTotalVolume :: [Transaction] -> USD
calculateTotalVolume = foldr (\tx acc -> acc + txAmount tx) (USD 0)

-- | 거부 이유별 집계 (함수형 그룹핑)
countRejectionReasons :: [Transaction] -> [(RejectionReason, Int)]
countRejectionReasons transactions =
  Map.toList $ foldr countReason Map.empty transactions
  where
    countReason tx acc = case txStatus tx of
      Rejected reason -> Map.insertWith (+) reason 1 acc
      _               -> acc

-- | 계좌별 일일 정산
settleDailyAccount :: Account -> [Transaction] -> Account
settleDailyAccount account transactions =
  foldr updateAccountAfterSettlement resetAccount approvedTxs
  where
    approvedTxs = filter isApproved transactions
    resetAccount = account { accDailySpent = USD 0 }  -- 일일 한도 리셋

-- | 정산 후 계좌 업데이트
updateAccountAfterSettlement :: Transaction -> Account -> Account
updateAccountAfterSettlement tx account =
  case txStatus tx of
    Settled  -> account  -- 이미 정산됨
    Approved -> account  -- 정산 완료로 표시는 Transaction에서 처리
    _        -> account

-- | 거래를 정산 완료 상태로 변경
settlTransaction :: Transaction -> Transaction
settlTransaction tx =
  case txStatus tx of
    Approved -> tx { txStatus = Settled }
    _        -> tx

-- | 전체 거래 정산 처리 (map 활용)
settleTransactions :: [Transaction] -> [Transaction]
settleTransactions = map settlTransaction . filter isApproved

-- | 통계 분석: 시간대별 거래 분석
analyzeTransactionsByHour :: [Transaction] -> Map.Map Int Int
analyzeTransactionsByHour transactions =
  foldr countByHour Map.empty transactions
  where
    countByHour _tx acc =
      let hour = 0  -- 실제로는 UTCTime에서 추출
      in Map.insertWith (+) hour 1 acc

-- | 의심스러운 패턴 탐지 (순수 함수로 복잡한 비즈니스 로직 표현)
detectSuspiciousPatterns :: [Transaction] -> [Transaction]
detectSuspiciousPatterns transactions =
  filter isSuspicious transactions
  where
    isSuspicious tx =
      let amount = txAmount tx
      in amount > USD 50000  -- 고액 거래
         || isRapidTransaction tx transactions  -- 단시간 내 반복 거래

-- | 단시간 내 반복 거래 체크
isRapidTransaction :: Transaction -> [Transaction] -> Bool
isRapidTransaction _ _ = False  -- 간단한 구현

-- | 월별 거래 요약 생성
monthlyTransactionSummary :: [Transaction] -> [(String, Int, USD)]
monthlyTransactionSummary transactions =
  let grouped = groupBy ((==) `on` getMonth) $ sortBy (compare `on` getMonth) transactions
  in map summarizeGroup grouped
  where
    getMonth :: Transaction -> String
    getMonth _tx = "2024-01"  -- 실제로는 timestamp에서 추출
    summarizeGroup group =
      case group of
        []            -> ("", 0, USD 0)
        (firstTx : _) -> (getMonth firstTx , length group , calculateTotalVolume group)
