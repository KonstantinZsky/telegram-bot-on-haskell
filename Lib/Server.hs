module Server where

import Data.Aeson (eitherDecode)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Data.Hashable (Hashable)
import qualified Data.Text as T

import Web.Telegram.HTTP (checkTelegramConnection)
import Data.Time.Extended (cpuToMicro)
import qualified Server.Monad   as S
import qualified Logger         as L
import qualified Web            as W
import qualified Web.Types      as W
import qualified Web.Parsing    as P
import qualified Control.Exception.Extends as E

runServer :: (S.MonadServer m, W.MonadWeb m, L.MonadLog m, E.MonadError m, S.MonadTime m,
    W.InputBotData m a b, W.SortingHashMap m h b, W.OutputBotData m b, Hashable b, Show b) => m ()
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

cycle_step :: (S.MonadServer m, W.MonadWeb m, L.MonadLog m, E.MonadError m, S.MonadTime m, 
    W.InputBotData m a b, W.SortingHashMap m h b, W.OutputBotData m b, Hashable b, Show b) => m ()
cycle_step = do
    jsonBody <- W.get
    botData <- P.parseInput jsonBody
    helpMsg <- S.getHelpMessage
    --L.debug $ T.pack $ show botData
    let messageHandling mType = case mType of
            (W.MessageText "/help")     -> W.AnswerInfo helpMsg
            (W.MessageText "/repeat")   -> W.AnswerButtons
            (W.Callback x)              -> W.SetRepeatCount x
            (W.MessageText txt)         -> W.AnswerText txt
    forPacking <- P.prepareOutput botData messageHandling
    forSending <- P.packOutput forPacking
    P.sendMessages forSending

      



        

