{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module TodoTypes
    where

import           Control.Lens.TH

import           Data.Default
import           Data.Text       (Text)

import           Monomer

import           TextShow

-- 데이터 모델 정의 (상태)

data TodoType = Home | Work | Sports
     deriving (Enum, Eq, Show)

instance TextShow TodoType where
  showt Home   = "Home"
  showt Work   = "Work"
  showt Sports = "Sports"

data TodoStatus = Pending | Done
     deriving (Enum, Eq, Show)

instance TextShow TodoStatus where
  showt Pending = "Pending"
  showt Done    = "Done"

data Todo = Todo { _todoId      :: Millisecond
                 , _todoType    :: TodoType
                 , _status      :: TodoStatus
                 , _description :: Text
                 }
     deriving (Eq, Show)

instance Default Todo where
  def = Todo { _todoId      = 0
             , _todoType    = Home
             , _status      = Pending
             , _description = ""
             }

data TodoAction = TodoNone
                | TodoAdding
                | TodoEditing Int
                | TodoConfirmingDelete Int Todo
     deriving (Eq, Show)

data TodoModel = TodoModel { _todos      :: [Todo]
                           , _activeTodo :: Todo
                           , _action     :: TodoAction
                           }
     deriving (Eq, Show)

data TodoEvt = TodoInit
             | TodoNew
             | TodoAdd
             | TodoEdit Int Todo
             | TodoSave Int
             | TodoConfirmDelete Int Todo
             | TodoCancelDelete
             | TodoDeleteBegin Int Todo
             | TodoDelete Int Todo
             | TodoShowEdit
             | TodoHideEdit
             | TodoHideEditDone
             | TodoCancel
     deriving (Eq, Show)

makeLenses 'TodoModel
makeLenses 'Todo

todoTypes :: [TodoType]
todoTypes = enumFrom (toEnum 0)

todoStatuses :: [TodoStatus]
todoStatuses = enumFrom (toEnum 0)
