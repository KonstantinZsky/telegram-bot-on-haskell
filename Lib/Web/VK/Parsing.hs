{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module Web.VK.Parsing where

import Data.Aeson
import Data.Text (Text, pack, unpack)
import Data.Foldable (asum)

import Web.Types

instance FromJSON VKBotData where

instance FromJSON VKBotMessage where
    parseJSON = withObject "message" $ \o ->
        asum[   do
            object <- o .: "object"
            message <- object .: "message"
            from_id <- message .: "from_id"
            text <- message .: "text"
            return $ VKBotMessage (MessageText text) (VKSupportData from_id),
            {-
                do
            cbq <- o .: "callback_query"
            message <- cbq .: "message"
            chat <- message .: "chat"
            chid <- chat .: "id" 
            callback_data_raw <- cbq .: "data" 
            let callback_data = read callback_data_raw
            --callback_data <- cbq .: "data"
            upid <- o .: "update_id"
            return $ TelegramBotMessage (Callback callback_data) (TelegramSupportData chid) upid,
            return $ UnknownMessageTG $ pack $ show o
            -}
            return $ UnknownMessageVK $ pack $ show o
            ]

instance FromJSON GetLongPollServer where
    parseJSON = withObject "message" $ \o -> do
        response <- o .: "response"
        key <- response .: "key"
        server <- response .: "server" 
        ts_raw <- response .: "ts" 
        let ts = read ts_raw
        return GetLongPollServer { key = key, server = server, tsGP = ts }