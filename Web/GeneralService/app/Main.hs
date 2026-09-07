{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Domain.Task
  ( Importance (..)
  , OutcomeOwner (..)
  , Status (..)
  , TaskItem (..)
  , TaskOwner (..)
  , Urgency (..)
  )
import Infrastructure.InMemoryTaskRepository (newInMemoryTaskRepository)
import Interface.Http.TaskRoutes (application)
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  repository <- newInMemoryTaskRepository initialTasks
  port <- maybe 3000 read <$> lookupEnv "PORT"
  putStrLn ("GeneralService is running at http://localhost:" <> show port)
  run port (application repository)

initialTasks :: [TaskItem]
initialTasks =
  [ TaskItem
      1
      "신규 계약서 검토"
      "법무 검토를 위한 초안을 준비했습니다."
      Draft
      Urgent
      Important
      (TaskOwner "김태스크")
      (OutcomeOwner "이아웃컴")
      "검토 의견이 반영된 계약서 초안"
      Nothing
      Nothing
  , TaskItem
      2
      "협력사 제안서"
      "검토 의견을 반영해 제출 준비 중입니다."
      Reviewed
      NotUrgent
      Important
      (TaskOwner "박태스크")
      (OutcomeOwner "이아웃컴")
      "협력사에 제출할 최종 제안서"
      Nothing
      (Just "검토 의견을 반영해 결과물을 제출해 주세요.")
  , TaskItem
      3
      "서비스 변경 합의서"
      "승인된 문서를 Outcome에 반영할 수 있습니다."
      Approved
      Urgent
      NotImportant
      (TaskOwner "최태스크")
      (OutcomeOwner "이아웃컴")
      "승인된 서비스 변경 합의서"
      (Just "서명 완료된 서비스 변경 합의서")
      (Just "승인되었습니다.")
  ]
