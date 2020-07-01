module Server where

import Data.Aeson (eitherDecode)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import qualified Data.Text as T

import Web.Telegram.HTTP (checkTelegramConnection)
import Data.Time.Extended (cpuToMicro)
import qualified Server.Monad   as S
import qualified Logger         as L
import qualified Web            as W
import qualified Web.Types      as W
import qualified Web.Parsing    as P
import qualified Control.Exception.Extends as E

runServer :: (S.MonadServer m, W.MonadWeb m, L.MonadLog m, E.MonadError m, S.MonadSortingHashTable m, S.MonadTime m) => m ()
runServer = do
    b <- S.getBotToken
    checkTelegramConnection b
    pollTimeout <- S.getPollTimeoutMicroseconds
    forever $ do
        timeStamp <- S.getCpuTimestamp
        cTime <- S.getCpuTime
        let timeToSleep = fromEnum pollTimeout - cpuToMicro (cTime - timeStamp)
        if (timeToSleep > 0)    then S.timeout timeToSleep
                                else L.warning $ "Poll timeout (" <> (T.pack $ show $ pollTimeout) <> 
                                    " microseconds) was exceeded. Actual timeout was: " <> 
                                    (T.pack $ show $ cpuToMicro (cTime - timeStamp)) <> 
                                    " microseconds. Too many messages."
        S.setCpuTimestamp
        cycle_step

cycle_step :: (S.MonadServer m, W.MonadWeb m, L.MonadLog m, E.MonadError m, S.MonadSortingHashTable m, S.MonadTime m) => m ()
cycle_step = do
    jsonBody <- W.get
    botData <- P.parseInput jsonBody
    helpMsg <- S.getHelpMessage
    let messageHandling mType = case mType of
            (W.MessageText "/help")     -> W.AnswerInfo helpMsg
            (W.MessageText "/repeat")   -> W.AnswerButtons
            (W.Callback x)              -> W.SetRepeatCount x
            (W.MessageText txt)         -> W.AnswerText txt
    forPacking <- P.prepareOutput botData messageHandling
    P.packOutput forPacking
    P.sendMessages

      



        

