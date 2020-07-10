{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Web.Types where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Data.Hashable (Hashable)

-- Telegram types

data Telegram
                  
type Chat_id = Integer                  

data TelegramSupportData = TelegramSupportData Chat_id deriving (Show, Eq, Generic) -- SupportData

instance Hashable TelegramSupportData where

data TelegramBotData = TelegramBotData { ok :: Bool, result :: [TelegramBotMessage]} deriving (Show, Generic)    

data TelegramBotMessage = TelegramBotMessage MessageType TelegramSupportData Update_id | UnknownMessageTG Text deriving (Show, Generic) 

-- VK types

data Vkontakte

type User_id = Integer 

data GetLongPollServer = GetLongPollServer { key :: Text, server :: Text, tsGP :: Integer} 

data VKSupportData = VKSupportData User_id deriving (Show, Eq, Generic)

instance Hashable VKSupportData where

data VKBotData = VKBotData { ts :: Text, updates :: [VKBotMessage]} deriving (Show, Generic)

data VKBotMessage = VKBotMessage MessageType VKSupportData | UnknownMessageVK Text deriving (Show, Generic)

-- Mutual types 

    -- input data
data MessageType = MessageText Text | Callback Integer deriving (Show, Eq, Generic, FromJSON)

type Update_id = Integer

data BotMessage a = BotMessage MessageType a deriving (Show, Eq, Generic) -- a = SupportData

    -- prepacked data
data AnswerType = AnswerInfo Text | AnswerText Text | AnswerButtons | SetRepeatCount Integer deriving Show

data BotAnswer a = BotAnswer AnswerType a deriving Show-- a = SupportData

    -- packed data
data KeyFlag = FlagText | FlagButtons deriving (Show, Eq, Generic)

instance Hashable KeyFlag where

data HashMapKey a = Key a KeyFlag deriving (Show, Eq, Generic) -- a = SupportData

instance (Hashable a) => Hashable (HashMapKey a) where -- a = SupportData

data HashMapData = DataText Text | Empty deriving Show
