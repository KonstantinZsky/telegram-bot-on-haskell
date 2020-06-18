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

import qualified Web as W
import qualified Server.Monad as S
import Control.Exception.Extends

checkTelegramConnection :: (MonadLog m, W.MonadWeb m, MonadError m) => T.Text -> m ()
checkTelegramConnection token = do
    r <- catchLogRethrow "Can not connect to the telegram bot. Possibly wrong token. Exiting program." W.get
    debug $ T.pack $ show r

handleMessages :: (S.MonadServer m, W.MonadWeb m) => BotData -> m ()
handleMessages BotData {result = []} = do
    S.setUpdateID (-1)
handleMessages btd = do
    let firstMsg = head $ result btd
    let upid = update_id firstMsg
    let outTxt = text $ message firstMsg
    let chatID = chat_id $ chat $ message firstMsg 
    let dt = object ["chat_id" .= chatID, "text" .= outTxt]
    W.post dt
    S.setUpdateID $ toEnum (upid+1)
