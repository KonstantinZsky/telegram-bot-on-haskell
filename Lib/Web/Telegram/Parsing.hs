module Web.Telegram.Parsing where

import Data.Aeson
import Data.Text (Text, pack, unpack)
import Data.Foldable (asum)
import Text.Read (readEither)

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
            upid <- o .: "update_id"
            let callback_data = readEither callback_data_raw
            case callback_data of
                (Right x) -> return $ TelegramBotMessage (Callback x) (TelegramSupportData chid) upid
                (Left msg) -> return $ UnknownMessageTG $ "Can't parse \"callback_data\": " <> (pack msg) <> " Whole message: " <> 
                    (pack $ show $ encode o),
            return $ UnknownMessageTG $ pack $ show $ encode o]


