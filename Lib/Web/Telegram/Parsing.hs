{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Web.Telegram.Parsing where

import GHC.Generics
import Data.Aeson
import Data.Text (Text, unpack)

data BotData = BotData { ok :: Bool, result :: [BotMessage]} deriving (Show, Generic, FromJSON)

data BotMessage = BotMessage { update_id :: Int, message :: MessageData} deriving (Show, Generic, FromJSON)

data MessageData = MessageData  { message_id :: Int
                                , from :: TelegramUser
                                , chat :: ChatData
                                , date :: Int
                                , text :: Text} deriving Show

instance FromJSON MessageData where
    parseJSON (Object v) =
        MessageData <$> v .: "message_id"
                    <*> v .: "from"
                    <*> v .: "chat"
                    <*> v .:? "date" .!= 0
                    <*> v .:? "text" .!= ""

data TelegramUser = TelegramUser    { user_id :: Int
                                    , is_bot :: Bool
                                    , first_name :: Text
                                    , username :: Text
                                    , language_code :: Text } deriving Show

instance FromJSON TelegramUser where
    parseJSON (Object v) =
        TelegramUser <$> v .:? "id" .!= 0
                     <*> v .:? "is_bot" .!= False
                     <*> v .:? "first_name" .!= ""
                     <*> v .:? "username" .!= ""
                     <*> v .:? "language_code" .!= ""      

data ChatData = ChatData    { chat_id :: Int
                            , first_name_chat :: Text
                            , username_chat :: Text
                            , chat_type :: Text } deriving Show

instance FromJSON ChatData where
    parseJSON (Object v) =
        ChatData <$> v .: "id"
                 <*> v .:? "first_name" .!= ""
                 <*> v .:? "username" .!= ""
                 <*> v .:? "type" .!= ""
