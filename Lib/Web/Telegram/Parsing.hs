{-# LANGUAGE ScopedTypeVariables #-}

module Web.Telegram.Parsing where

import Data.Aeson
import Data.Text (Text, pack, unpack)
import Data.Foldable (asum)

import Web.Types

instance FromJSON TelegramBotData

instance FromJSON TelegramBotMessage where
    parseJSON = withObject "message" $ \o ->
        asum[   do
            message <- o .: "message"
            chat <- message .: "chat"
            chid <- chat .: "id" 
            message_txt <- message .: "text" 
            upid <- o .: "update_id"
            return TelegramBotMessage {messageType = MessageText message_txt, chat_id = chid, update_id = upid},
                do
            cbq <- o .: "callback_query"
            message <- cbq .: "message"
            chat <- message .: "chat"
            chid <- chat .: "id" 
            callback_data <- cbq .: "data" 
            upid <- o .: "update_id"
            return TelegramBotMessage {messageType = Callback callback_data, chat_id = chid, update_id = upid},
            return $ UnknownMessage $ pack $ show o]
