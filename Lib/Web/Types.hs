{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Web.Types where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)

-- Input data

data BotData = BotData TelegramBotData | VkBotData -- VK not implemented yet

data TelegramBotData = TelegramBotData { ok :: Bool, result :: [TelegramBotMessage]} deriving (Show, Generic)

data MessageType = MessageText Text | Callback Int deriving (Show, Eq, Generic, FromJSON)

data TelegramBotMessage = TelegramBotMessage { messageType :: MessageType, chat_id :: Integer, update_id :: Integer } |
                    UnknownMessage Text deriving (Show, Generic)

-- Output data

data AnswerType = AnswerText Text | AnswerButtons
                                    -- chat_id
data SupportData = TelegramSupportData Integer | VKSupportData -- VK not implemented yet

data BotAnswer = BotAnswer AnswerType SupportData