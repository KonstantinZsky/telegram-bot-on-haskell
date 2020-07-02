module Web.Parsing where

import Data.ByteString.Lazy (ByteString)
import Data.Aeson (eitherDecode)
import Control.Monad (guard, when)
import qualified Data.Text as T

import Data.Time.Extended (secondsToCPU, cpuToMicro)
import Web.Telegram.Parsing
import Config.Mode (Mode(..))
import qualified Web.Telegram.HTTP (handleMessage)
import qualified Web.Types      as W
import qualified Web            as W
import qualified Logger         as L
import qualified Server.Monad   as S
import qualified Control.Exception.Extends as E


parseInput :: (Monad m, W.MonadWeb m, E.MonadError m) => ByteString -> m W.BotData
parseInput input = do
    mode <- W.getMode  
    let bot_data = case mode of
                        TG -> (eitherDecode input :: Either String W.TelegramBotData)
                        VK -> undefined -- not implemented yet
    case bot_data of
        (Left err)  -> E.errorThrow $ "Impossible parsing error while processing bot answer: " <> (T.pack $ show err)   
        (Right btd) -> return $ W.BotData btd

-- data BotData = BotData TelegramBotData | VkBotData 
-- data TelegramBotData = TelegramBotData { ok :: Bool, result :: [TelegramBotMessage]} deriving (Show, Generic)
-- data TelegramBotMessage = TelegramBotMessage { messageType :: MessageType, chat_id :: Integer, update_id :: Integer } |
--                    UnknownMessage Text

-- data MessageType = MessageText Text | Callback Int
prepareOutput :: (Monad m, E.MonadError m, L.MonadLog m, S.MonadServer m) => W.BotData -> 
                        (W.MessageType -> W.AnswerType) -> m [W.BotAnswer]
prepareOutput (W.VkBotData) _ = undefined -- not implemented yet
prepareOutput (W.BotData (W.TelegramBotData {W.ok = False, W.result = result})) _ = 
    E.errorThrow $ "Telegram response - not ok, message: " <> (T.pack $ show result)
prepareOutput (W.BotData (W.TelegramBotData {W.result = []})) _ = return []
prepareOutput (W.BotData (W.TelegramBotData {W.result = result})) processing = do
    let warnings = [ x | x@(W.UnknownMessage _) <- result ]
        upids = [ update_id | (W.TelegramBotMessage {W.update_id = update_id}) <- result ]
        answers = do
            W.TelegramBotMessage {W.messageType = messageType, W.chat_id = chat_id} <- result
            return $ W.BotAnswer (processing messageType) $ W.TelegramSupportData chat_id
    mapM_ (\(W.UnknownMessage txt) -> L.warning $ "Unknown format of telegram message, will be ignored: " <> txt) warnings
    when (upids /= []) $ S.setUpdateID $ 1 + maximum upids
    L.debug $ T.pack $ show answers
    return answers

-- temporary here, will be moved
packOutput :: (Monad m, L.MonadLog m, S.MonadServer m, S.MonadSortingHashTable m) => [W.BotAnswer] -> m ()
packOutput answers = mapM_ (func) answers where
    func (W.BotAnswer ansT supD) = case ansT of
        (W.SetRepeatCount x) -> do 
            S.setRepeatCount x 
            L.debug $ "Repeat count changed: " <> (T.pack $ show x) <> ". packOutput 1"
        (W.AnswerText txt) -> do 
            --L.debug $ "packOutput AnswerText txt: " <> (T.pack $ show txt)
            rc <- S.getRepeatCount
            rc_correct <- if rc < 1 then do
                    L.warning $ "Wrong repeatCount: " <> (T.pack $ show rc) <> ". Must be > 0, will be set to 1."
                    S.setRepeatCount 1
                    --L.debug $ "Repeat count changed: " <> (T.pack $ show 1) <> ". packOutput 2"
                    return 1 
                else return rc 
            S.alter (W.Key supD W.FlagText) (\ansOld -> case ansOld of
                (Just (W.DataText txtOld)) -> Just $ W.DataText $ txtOld <> (T.replicate (fromEnum rc_correct) $ txt <> "\n")
                _       -> Just $ W.DataText $ T.replicate (fromEnum rc_correct) (txt <> "\n"))
        (W.AnswerInfo txt) -> S.alter (W.Key supD W.FlagText) (\ansOld -> case ansOld of
                (Just (W.DataText txtOld)) -> Just $ W.DataText $ txtOld <> txt <> "\n"
                _       -> Just $ W.DataText $ txt <> "\n")
        (W.AnswerButtons) -> do
            --L.debug $ "packOutput AnswerButtons supD: " <> (T.pack $ show supD)
            S.alter (W.Key supD W.FlagButtons) $ \_ -> Just W.Empty

sendMessages :: (Monad m, L.MonadLog m, S.MonadServer m, S.MonadSortingHashTable m, S.MonadTime m, W.MonadWeb m) => m ()
sendMessages = do
    packedMessages <- S.toList
    --L.debug $ T.pack $ show packedMessages
    cpuTime <- S.getCpuTime
    sendMessagesCycle packedMessages $ cpuTime - secondsToCPU 1
    S.emptyHashTable
    return ()

sendMessagesCycle :: (Monad m, S.MonadServer m, S.MonadTime m, W.MonadWeb m) => 
                        [(W.HashMapKey, W.HashMapData)] -> Integer -> m ()
sendMessagesCycle [] _ = return ()
sendMessagesCycle xs tStamp = do
    cpuTime <- S.getCpuTime
    let timeToSleep = secondsToCPU 1 - (cpuTime - tStamp)
    when (timeToSleep > 0) $ S.timeout $ cpuToMicro timeToSleep 
    freq <- S.getMaximumMessageFrequency
    let (now, next) = splitAt (fromEnum freq) xs
    mapM_ Web.Telegram.HTTP.handleMessage now
    sendMessagesCycle next cpuTime

