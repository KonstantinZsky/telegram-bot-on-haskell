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
            return $ TelegramBotMessage (MessageText message_txt) (TelegramSupportData chid) upid,
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
            return $ UnknownMessageTG $ pack $ show o]


