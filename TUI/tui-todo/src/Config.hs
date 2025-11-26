{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Configuration management (MIXED: Pure + Effectful)
--
-- This module handles keybindings configuration.
--
-- Pure functions:
--   - keyToString: Convert Vty key to string
--   - matchesKey: Match key to action
--   - getFirstKey: Get first key from list
--   - defaultKeyBindings: Default configuration
--
-- Effectful functions:
--   - loadKeyBindings: Load from YAML file (IO)
--   - loadKeyBindingsWithMessages: Load with custom messages (IO)
--
-- Effects:
--   - File system access (doesFileExist, readFile)
--   - YAML parsing
--   - Console output (putStrLn)
module Config
    ( KeyAction (..)
    , KeyBindings (..)
    , defaultKeyBindings
    , getFirstKey
    , loadKeyBindings
    , loadKeyBindingsWithMessages
    , matchesKey
    ) where

import           Data.Aeson       (FromJSON, parseJSON, withObject, (.:))
import qualified Data.ByteString  as BS
import           Data.List        (find)
import qualified Data.Yaml        as Yaml

import           Flow             ((<|))

import           GHC.Generics     (Generic)

import qualified Graphics.Vty     as V

import qualified I18n

import           System.Directory (doesFileExist)

-- | Key action types (Pure)
data KeyAction = QuitApp | AddTodo | ToggleComplete | DeleteTodo | NavigateUp | NavigateDown | SaveInput | CancelInput
     deriving (Eq, Generic, Show)

-- | Key bindings configuration (Pure)
data KeyBindings = KeyBindings { quit            :: ![String]
                               , add_todo        :: ![String]
                               , toggle_complete :: ![String]
                               , delete_todo     :: ![String]
                               , navigate_up     :: ![String]
                               , navigate_down   :: ![String]
                               , save_input      :: ![String]
                               , cancel_input    :: ![String]
                               }
     deriving (Generic, Show)

instance FromJSON KeyBindings where
  parseJSON =
    withObject "KeyBindings" <| \v -> do
      kb <- v .: "keybindings"
      KeyBindings
        <$> kb .: "quit"
        <*> kb .: "add_todo"
        <*> kb .: "toggle_complete"
        <*> kb .: "delete_todo"
        <*> kb .: "navigate_up"
        <*> kb .: "navigate_down"
        <*> kb .: "save_input"
        <*> kb .: "cancel_input"

-- | Default key bindings (Pure)
defaultKeyBindings :: KeyBindings
defaultKeyBindings =
  KeyBindings
    { quit = ["q", "Esc"],
      add_todo = ["a"],
      toggle_complete = ["Space"],
      delete_todo = ["d"],
      navigate_up = ["Up", "k"],
      navigate_down = ["Down", "j"],
      save_input = ["Enter"],
      cancel_input = ["Esc"]
    }

-- | Load key bindings from configuration file(Effectful)
loadKeyBindings :: FilePath -> IO KeyBindings
loadKeyBindings path = loadKeyBindingsWithMessages path I18n.defaultMessages

-- | Load key bindings with custom messages(Effectful)
loadKeyBindingsWithMessages :: FilePath -> I18n.I18nMessages -> IO KeyBindings
loadKeyBindingsWithMessages path msgs = do
  let sysMsgs = I18n.messages msgs
  exists <- doesFileExist path
  if exists
    then loadFromFile sysMsgs
    else useDefault sysMsgs <| I18n.config_not_found sysMsgs <> ": " <> path
  where
    loadFromFile sysMsgs = do
      content <- BS.readFile path
      case Yaml.decodeEither' content of
        Left err -> useDefault sysMsgs <| I18n.config_load_failed sysMsgs <> ": " <> show err
        Right kb -> do
          putStrLn <| I18n.config_loaded sysMsgs
          pure kb

    useDefault sysMsgs msg = do
      putStrLn msg
      putStrLn <| I18n.using_default sysMsgs
      pure defaultKeyBindings

-- | Convert Vty key to string representation (Pure)
keyToString :: V.Key -> String
keyToString = \case
  V.KChar ' ' -> "Space"
  V.KChar c -> [c]
  V.KEnter -> "Enter"
  V.KEsc -> "Esc"
  V.KUp -> "Up"
  V.KDown -> "Down"
  V.KLeft -> "Left"
  V.KRight -> "Right"
  V.KBS -> "Backspace"
  V.KFun n -> "F" <> show n
  _ -> ""

-- | Match a key to its corresponding action (Pure)
matchesKey :: KeyBindings -> V.Key -> Maybe KeyAction
matchesKey kb key = snd <$> find (keyMatches keyStr . fst) actions
  where
    keyStr = keyToString key
    keyMatches str keys = str `elem` keys
    actions =
      [ (quit kb, QuitApp),
        (add_todo kb, AddTodo),
        (toggle_complete kb, ToggleComplete),
        (delete_todo kb, DeleteTodo),
        (navigate_up kb, NavigateUp),
        (navigate_down kb, NavigateDown),
        (save_input kb, SaveInput),
        (cancel_input kb, CancelInput)
      ]

-- | Get the first key from a list of keys, or return a default (Pure)
getFirstKey :: [String] -> String -> String
getFirstKey [] def     = def
getFirstKey (k:_) _def = k
