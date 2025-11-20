{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
    ( Account (..)
    , AccountId (..)
    , KRW (..)
    , RejectionReason (..)
    , SettlementReport (..)
    , Transaction (..)
    , TransactionId (..)
    , TransactionStatus (..)
    , TransactionType (..)
    , USD (..)
    , ValidationResult
    ) where

import qualified Data.Text as T
import           Data.Time (UTCTime)

-- | USD 화폐 단위를 타입으로 구분 (컴파일 타임에 오류 방지)
newtype USD = USD Double
     deriving (Eq, Num, Ord, Show)

-- | KRW 화폐 단위를 타입으로 구분 (컴파일 타임에 오류 방지)
newtype KRW = KRW Double
     deriving (Eq, Num, Ord, Show)

-- | 거래 ID (타입 안정성)
newtype TransactionId = TransactionId T.Text
     deriving (Eq, Ord, Show)

-- | 계좌 번호
newtype AccountId = AccountId T.Text
     deriving (Eq, Ord, Show)

-- | 거래 타입 (대수적 데이터 타입)
data TransactionType = Deposit -- 입금
                     | Withdrawal -- 출금
                     | Transfer AccountId -- 송금 대상 계좌
                     | Payment T.Text -- 결제 (가맹점명)
     deriving (Eq, Show)

-- | 거래 상태 (불가능한 상태 조합을 타입으로 방지)
data TransactionStatus = Pending -- 보류중
                       | Approved -- 승인됨
                       | Rejected RejectionReason -- 거부됨
                       | Settled -- 정산됨
     deriving (Eq, Show)

-- | 거래 거부 이유
data RejectionReason = InsufficientFunds -- 자금 부족
                     | DailyLimitExceeded -- 일일 한도 초과
                     | SuspiciousActivity -- 의심스러운 활동
                     | InvalidAccount -- 잘못된 계좌
                     | InvalidAmount -- 잘못된 금액
     deriving (Eq, Ord, Show)

-- | 거래 기록 (불변 데이터)
data Transaction = Transaction { txId        :: TransactionId
                                 -- ^거래ID
                               , txFrom      :: AccountId
                                 -- ^계좌ID
                               , txType      :: TransactionType
                                 -- ^거래유형
                               , txAmount    :: USD
                                 -- ^USD 금액
                               , txTimestamp :: UTCTime
                                 -- ^거래시각
                               , txStatus    :: TransactionStatus
                                 -- ^거래상태
                               }
     deriving (Eq, Show)

-- | 계좌 정보
data Account = Account { accId         :: AccountId
                         -- ^계좌ID
                       , accBalance    :: USD
                         -- ^계좌 잔액
                       , accDailyLimit :: USD
                         -- ^계좌 일일 한도
                       , accDailySpent :: USD
                         -- ^계좌 일일 지출
                       }
     deriving (Eq, Show)

-- | 검증 결과 (Either를 사용한 함수형 에러 처리)
type ValidationResult = Either RejectionReason Transaction

-- | 정산 보고서
data SettlementReport = SettlementReport { reportDate :: UTCTime
                                           -- ^보고서 작성일
                                         , totalTransactions :: Int
                                           -- ^총거래량
                                         , totalApproved :: Int
                                           -- ^총승인된 건수
                                         , totalRejected :: Int
                                           -- ^총거부된 건수
                                         , totalVolume :: USD
                                           -- ^총금액
                                         , rejectionReasons :: [(RejectionReason, Int)]
                                           -- ^거부사유
                                         }
     deriving (Eq, Show)
