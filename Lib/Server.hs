module Server where

import Data.Aeson (eitherDecode)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import qualified Data.Text as T

import Web.Telegram.HTTP (handleMessages, checkTelegramConnection)
import qualified Server.Monad   as S
import qualified Logger         as L
import qualified Web            as W
import qualified Control.Exception.Extends as E

runServer :: (S.MonadServer m, S.MonadTimeout m, W.MonadWeb m, L.MonadLog m, E.MonadError m) => m ()
runServer = do
    b <- S.getBotToken
    checkTelegramConnection b
    pollTimeout <- S.getPollTimeoutMicroseconds
    forever $ do
        S.timeout $ fromEnum pollTimeout
        cycle_step

cycle_step :: (S.MonadServer m, W.MonadWeb m, L.MonadLog m, E.MonadError m) => m ()
cycle_step = do
    jsonBody <- W.get
    let bot_data = eitherDecode jsonBody
    upid <- S.getUpdateID
    --L.debug $ "Step, upid: " <> (T.pack $ show upid)
    case bot_data of
        (Right bdt) -> do
            handleMessages bdt -- bdt will not be :: BotData for VK, need to think it out
        (Left err) -> do
            L.warning $ "Unsupported telegram message: " <> T.pack err
            --upid2 <- S.getUpdateID
            --L.info $ "Unsupported message, upid: " <> (T.pack $ show upid2)
            --L.debug $ "Unsupported message, upid: " <> (T.pack $ show upid2)
            --L.error $ "Unsupported message, upid: " <> (T.pack $ show upid2)
            S.setUpdateID $ upid + 1
            --E.errorThrow $ T.pack err


      



        

