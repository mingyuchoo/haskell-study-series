{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Application.TaskService as TaskService
import Domain.Task
import Infrastructure.InMemoryTaskRepository (newInMemoryTaskRepository)
import System.Exit (exitFailure)

main :: IO ()
main = do
  let input =
        TaskInput
          "업무"
          "설명"
          Submitted
          Urgent
          Important
          (TaskOwner "담당자")
          (OutcomeOwner "성과 담당자")
          "완료된 보고서"
      created = createTask 7 input
      updated =
        updateTask
          created
          ( TaskInput
              "수정 업무"
              "수정 설명"
              Approved
              NotUrgent
              NotImportant
              (TaskOwner "담당자")
              (OutcomeOwner "성과 담당자")
              "수정된 보고서"
          )
  assert "생성 시 ID와 상태를 보존한다" (taskId created == 7 && status created == Submitted)
  assert "수정 시 내용과 상태를 갱신한다" (title updated == "수정 업무" && status updated == Approved)
  assert
    "수정 시 우선순위를 갱신한다"
    (urgency updated == NotUrgent && importance updated == NotImportant)
  assert
    "긴급도와 중요도를 사분면으로 변환한다"
    ( quadrantOf created == DoFirst
        && toQuadrant NotUrgent Important == Schedule
        && toQuadrant Urgent NotImportant == Delegate
        && toQuadrant NotUrgent NotImportant == Eliminate
    )
  assert
    "빈 제목을 거부한다"
    ( validateTaskInput
        ( TaskInput
            "  "
            "설명"
            Draft
            NotUrgent
            Important
            (TaskOwner "담당자")
            (OutcomeOwner "성과 담당자")
            "결과"
        )
        == Left EmptyTitle
    )
  repository <- newInMemoryTaskRepository []
  createdInRepository <- TaskService.createTask repository input
  assert
    "유스케이스가 유효한 업무를 생성한다"
    ( createdInRepository
        == Right
          ( TaskItem
              1
              "업무"
              "설명"
              Submitted
              Urgent
              Important
              (TaskOwner "담당자")
              (OutcomeOwner "성과 담당자")
              "완료된 보고서"
              Nothing
              Nothing
          )
    )
  deleted <- TaskService.deleteTask repository 1
  assert "유스케이스가 존재하는 업무를 삭제한다" deleted
  putStrLn "All domain tests passed"

assert :: String -> Bool -> IO ()
assert label condition =
  if condition
    then putStrLn ("PASS: " <> label)
    else putStrLn ("FAIL: " <> label) >> exitFailure
