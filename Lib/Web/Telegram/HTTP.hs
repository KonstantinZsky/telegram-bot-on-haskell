{-# LANGUAGE QuasiQuotes #-}

module Web.Telegram.HTTP where

import Data.IORef
import Control.Lens hiding ( (.=) )
import Network.Wreq
import Network.Wreq.Types
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (MonadIO, liftIO) 
import qualified Network.Wreq.Session as S
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified System.Exit    as E
import Prelude hiding (log)

import Web.Telegram.Parsing
import Logger (MonadLog, debug)
import qualified Files as F
import Env

import Data.Aeson
import Data.Aeson.QQ

import qualified Web            as W
import qualified Web.Types      as W
import qualified Server.Monad   as S
import Control.Exception.Extends

checkTelegramConnection :: (MonadLog m, W.MonadWeb m, MonadError m) => T.Text -> m ()
checkTelegramConnection token = do
    r <- catchLogRethrow "Can not connect to the telegram bot. Possibly wrong token. Exiting program." W.get
    debug $ T.pack $ show r


handleMessage :: (S.MonadServer m, W.MonadWeb m) => (W.SupportData, W.AnswerType) -> m ()
handleMessage (sd, at) = do
    askRepeatMsg <- S.getRepeateQuestion
    let buttons = [aesonQQ| {inline_keyboard: [[{text: "1", callback_data: 1},{text: "2", callback_data: 2},{text: "3", callback_data: 3},{text: "4", callback_data: 4},{text: "5", callback_data: 5}]]}  |] :: Value
    let dt = case sd of 
            (W.TelegramSupportData chatID)    -> case at of
                (W.AnswerText outTxt)   -> object ["chat_id" .= chatID, "text" .= outTxt]
                (W.AnswerInfo outTxt)   -> object ["chat_id" .= chatID, "text" .= outTxt]
                (W.AnswerButtons)       -> object ["chat_id" .= chatID, "text" .= askRepeatMsg, "reply_markup" .= buttons]
                (W.SetRepeatCount _)    -> undefined -- error, must change types to avoid this situation           
            W.VKSupportData                   -> undefined -- not implemented yet
    W.post dt -- ignoring social network answer for now
    return ()
