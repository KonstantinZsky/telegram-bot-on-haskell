{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Web.Types where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Data.Hashable (Hashable)

-- Input data

data BotData = BotData TelegramBotData | VkBotData deriving Show-- VK not implemented yet

data TelegramBotData = TelegramBotData { ok :: Bool, result :: [TelegramBotMessage]} deriving (Show, Generic)

data MessageType = MessageText Text | Callback Integer deriving (Show, Eq, Generic, FromJSON)

data TelegramBotMessage = TelegramBotMessage { messageType :: MessageType, chat_id :: Integer, update_id :: Integer } |
                    UnknownMessage Text deriving (Show, Generic)

-- Prepacked data

data AnswerType = AnswerText Text | AnswerInfo Text | AnswerButtons | SetRepeatCount Integer deriving Show
                                    -- chat_id
data SupportData = TelegramSupportData Integer | VKSupportData deriving (Show, Eq, Generic)-- VK not implemented yet

instance Hashable SupportData where

data BotAnswer = BotAnswer AnswerType SupportData deriving Show

-- Packed data

data KeyFlag = FlagText | FlagButtons deriving (Show, Eq, Generic)

instance Hashable KeyFlag where

data HashMapKey = Key SupportData KeyFlag deriving (Show, Eq, Generic)

instance Hashable HashMapKey where

data HashMapData = DataText Text | Empty deriving Show