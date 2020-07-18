{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module Web.VK.Parsing where

import Data.Aeson
import Data.Text (Text, pack, unpack)
import Data.Foldable (asum)
import Text.Read (readEither)
import qualified Data.Text as T

import Web.Types

instance FromJSON VKBotData where

instance FromJSON VKBotMessage where
    parseJSON = withObject "message" $ \o ->
        asum[   do
            object <- o .: "object"
            message <- object .: "message"
            from_id <- message .: "from_id"
            payload <- message .: "payload"
            --button_raw <- payload .: "button"
            let button = readEither $ T.unpack $ supportParsing payload
            case button of 
                (Right x) -> return $ VKBotMessage (Callback x) (VKSupportData from_id)
                (Left msg) -> return $ UnknownMessageVK $ T.pack msg <> " PS: " <> (supportParsing payload),      
                do
            object <- o .: "object"
            message <- object .: "message"
            from_id <- message .: "from_id"
            text <- message .: "text"
            return $ VKBotMessage (MessageText text) (VKSupportData from_id), {--}
            return $ UnknownMessageVK $ pack $ show $ encode o
            ]

instance FromJSON GetLongPollServer where
    parseJSON = withObject "message" $ \o -> do
        response <- o .: "response"
        key <- response .: "key"
        server <- response .: "server" 
        ts_raw <- response .: "ts"
        let ts = read ts_raw
        return GetLongPollServer { key = key, server = server, tsGP = ts }

supportParsing :: T.Text -> T.Text
supportParsing str = T.dropEnd 1 $ snd $ T.breakOnEnd  ":" str