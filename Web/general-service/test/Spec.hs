{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Application.AuthService as AuthService
import qualified Application.TaskService as TaskService
import Domain.Task
import Domain.User
  ( AuthError (..)
  , ProfileUpdate (..)
  , SignUpInput (..)
  , profileDisplayName
  )
import Infrastructure.InMemoryTaskRepository (newInMemoryTaskRepository)
import Infrastructure.InMemoryUserRepository (newInMemoryUserRepository)
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
  userRepository <- newInMemoryUserRepository
  signedUp <-
    AuthService.signUp
      userRepository
      (SignUpInput "member@example.com" "새 사용자" "safe-password")
  session <- expectRight "유효한 사용자를 가입시킨다" signedUp
  duplicate <-
    AuthService.signUp
      userRepository
      (SignUpInput "member@example.com" "다른 사용자" "safe-password")
  assert "중복 이메일 가입을 거부한다" (duplicate == Left DuplicateEmail)
  invalidLogin <- AuthService.login userRepository "member@example.com" "wrong-password"
  assert "잘못된 비밀번호 로그인을 거부한다" (invalidLogin == Left InvalidCredentials)
  loggedIn <- AuthService.login userRepository "member@example.com" "safe-password"
  loginSession <- expectRight "올바른 비밀번호로 로그인한다" loggedIn
  updatedProfile <-
    AuthService.updateProfile
      userRepository
      (AuthService.sessionToken loginSession)
      (ProfileUpdate "변경된 사용자")
  profile <- expectRight "로그인한 사용자가 프로필을 수정한다" updatedProfile
  assert "표시 이름을 갱신한다" (profileDisplayName profile == "변경된 사용자")
  AuthService.logout userRepository (AuthService.sessionToken session)
  loggedOut <- AuthService.currentUser userRepository (AuthService.sessionToken session)
  assert "로그아웃한 세션을 거부한다" (loggedOut == Left AuthenticationRequired)
  putStrLn "All domain tests passed"

assert :: String -> Bool -> IO ()
assert label condition =
  if condition
    then putStrLn ("PASS: " <> label)
    else putStrLn ("FAIL: " <> label) >> exitFailure

expectRight :: String -> Either a b -> IO b
expectRight label result =
  case result of
    Right value -> putStrLn ("PASS: " <> label) >> pure value
    Left _ -> putStrLn ("FAIL: " <> label) >> exitFailure
