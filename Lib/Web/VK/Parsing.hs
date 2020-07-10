{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module Web.VK.Parsing where

import Data.Aeson
import Data.Text (Text, pack, unpack)
import Data.Foldable (asum)

import Web.Types

instance FromJSON GetLongPollServer where
    parseJSON = withObject "message" $ \o -> do
        response <- o .: "response"
        key <- response .: "key"
        server <- response .: "server" 
        ts_raw <- response .: "ts" 
        let ts = read ts_raw
        return GetLongPollServer { key = key, server = server, ts = ts }