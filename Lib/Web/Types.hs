{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Web.Types where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)

data BotData = BotData TelegramBotData | VkBotData -- VK not implemented yet

data TelegramBotData = TelegramBotData { ok :: Bool, result :: [TelegramBotMessage]} deriving (Show, Generic)

data TelegramMessageType = MessageText Text | Callback Int deriving (Show, Generic, FromJSON)

data TelegramBotMessage = TelegramBotMessage { messageType :: TelegramMessageType, chat_id :: Integer, update_id :: Integer } |
                    UnknownMessage Text deriving (Show, Generic)