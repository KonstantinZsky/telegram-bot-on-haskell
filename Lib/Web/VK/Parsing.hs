{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Web.VK.Parsing where

import GHC.Generics
import Data.Aeson
import Data.Text (Text, pack, unpack)
import Data.Foldable (asum)
import Text.Read (readEither)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text as T

import Web.Types

instance FromJSON VKBotData

instance FromJSON VKBotMessage where
    parseJSON = withObject "message" $ \o ->
        asum[   do
            object <- o .: "object"
            message <- object .: "message"
            from_id <- message .: "from_id"
            text <- message .: "text"
            asum[   do
                payload <- message .: "payload"
                let button = decode $ T.encodeUtf8 payload
                case button of 
                    (Just GetButton{button = x}) -> return $ VKBotMessage (Callback x) (VKSupportData from_id)
                    Nothing -> return $ UnknownMessageVK $ "Can't decode \"payload\". Whole message: " <> (pack $ show $ encode o),                    
                return $ VKBotMessage (MessageText text) (VKSupportData from_id)
                ],      
            return $ UnknownMessageVK $ pack $ show $ encode o
            ]

instance FromJSON GetLongPollServer where
    parseJSON = withObject "message" $ \o -> 
        asum[   do
            response <- o .: "response"
            key <- response .: "key"
            server <- response .: "server" 
            ts_raw <- response .: "ts"
            let ts = readEither ts_raw
            case ts of 
                (Right x) -> return GetLongPollServer { key = key, server = server, tsGP = x }
                (Left msg) -> fail $ "Parsing problems: " <> msg <> " Whole message: " <> (show $ encode o),
            fail $ show $ encode o
            ]

newtype GetButton = GetButton {button :: Integer} deriving (Show, Generic, FromJSON) 