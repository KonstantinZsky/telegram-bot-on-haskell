{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, QuasiQuotes #-}

module Web.Telegram.Instances where

import qualified Data.Text    as T
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as Sess
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.IO.Class (liftIO) 
import Data.Maybe (fromMaybe)
import Control.Lens ((^?))
import Data.Aeson
import Data.Aeson.QQ

import qualified Server.Monad   as S
import qualified Env as E
import qualified Web.Types as WebT
import Config.Mode (Mode(..))
import Web.Classes (MonadWeb(..),OutputBotData(..))

instance MonadWeb (ReaderT (E.Env WebT.Telegram) IO) where
    getSession = ReaderT E.getSession
    get = do
        uid_in <- S.getUpdateID
        b <- T.unpack <$> S.getBotToken        
        let conStr = case uid_in of
                (-1)    -> "https://api.telegram.org/bot" <> b <> "/getUpdates"
                _       -> "https://api.telegram.org/bot" <> b <> "/getUpdates?offset=" <> (show uid_in)
        s <- getSession
        r <- liftIO $ Sess.get s conStr
        return $ fromMaybe "" $ r ^? W.responseBody
    post json = do
        b <- T.unpack <$> S.getBotToken
        let conStr =  "https://api.telegram.org/bot" <> b <> "/sendMessage"
        s <- getSession
        liftIO $ Sess.post s conStr json

instance OutputBotData (ReaderT (E.Env WebT.Telegram) IO) WebT.TelegramSupportData where
    handleMessage (hk, hd) = do
        askRepeatMsg <- S.getRepeateQuestion
        let buttons = [aesonQQ| {inline_keyboard: [[{text: "1", callback_data: 1},{text: "2", callback_data: 2},{text: "3", callback_data: 3},{text: "4", callback_data: 4},{text: "5", callback_data: 5}]]}  |] :: Value
        let dt = case hk of 
                (WebT.Key (WebT.TelegramSupportData chatID) WebT.FlagText)    -> case hd of
                    (WebT.DataText outTxt)     -> object ["chat_id" .= chatID, "text" .= outTxt]
                    WebT.Empty                 -> object ["chat_id" .= chatID, "text" .= ("" :: T.Text)]        
                (WebT.Key (WebT.TelegramSupportData chatID) WebT.FlagButtons) -> 
                    object ["chat_id" .= chatID, "text" .= askRepeatMsg, "reply_markup" .= buttons]
        post dt -- ignoring social network answer for now
        return ()
