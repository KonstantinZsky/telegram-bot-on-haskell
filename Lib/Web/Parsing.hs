{-# LANGUAGE FlexibleContexts #-}

module Web.Parsing where

import Data.ByteString.Lazy (ByteString)
import Data.Hashable (Hashable)
import Data.Aeson (eitherDecode)
import Control.Monad (guard, when)
import qualified Data.Text as T

import Data.Time.Extended (secondsToCPU, cpuToMicro)
import Web.Telegram.Parsing
import qualified Web.Types      as W
import qualified Web            as W
import qualified Logger         as L
import qualified Server.Monad   as S
import qualified Control.Exception.Extends as E

parseInput :: (Monad m, W.MonadWeb m, E.MonadError m, W.InputBotData m a b) => ByteString -> m a
parseInput input = do 
    bot_data <- W.decode input
    case bot_data of
        (Left err)  -> E.errorThrow $ "Impossible parsing error while processing bot answer: " <> (T.pack $ show err)   
        (Right btd) -> return btd        

prepareOutput :: (Monad m, E.MonadError m, L.MonadLog m, S.MonadServer m, W.InputBotData m a b, W.SortingHashMap m h b, 
    Hashable b, Show b) => a -> (W.MessageType -> W.AnswerType) -> m [W.BotAnswer b]
prepareOutput botD processing = do
    (ok, errMsg) <- W.messageStatus botD
    when (not ok) $ E.errorThrow errMsg
    --checkEmpty <- W.messageEmpty botD -- may be not needed ????
    --if checkEmpty then (return []) else do
    (warnings, upids, answersRaw) <- W.messageData botD
    let answers = [W.BotAnswer (processing mesD) $ supD | W.BotMessage mesD supD _ <- answersRaw]
    mapM_ (\txt -> L.warning txt) warnings
    when (upids /= []) $ S.setUpdateID $ 1 + maximum upids
    L.debug $ T.pack $ show answers
    return answers

packOutput :: (Monad m, L.MonadLog m, S.MonadServer m, W.InputBotData m a b, W.SortingHashMap m h b, Hashable b) => 
                    [W.BotAnswer b] -> m [(W.HashMapKey b, W.HashMapData)]
packOutput answers = do
    hashMap <- W.createHashMap
    let func = (\(W.BotAnswer ansT supD) -> case ansT of
            (W.SetRepeatCount x) -> do 
                S.setRepeatCount x 
                L.debug $ "Repeat count changed: " <> (T.pack $ show x) <> ". packOutput 1"
            (W.AnswerText txt) -> do 
                rc <- S.getRepeatCount
                rc_correct <- if rc < 1 then do
                        L.warning $ "Wrong repeatCount: " <> (T.pack $ show rc) <> ". Must be > 0, will be set to 1."
                        S.setRepeatCount 1
                        return 1 
                    else return rc 
                W.alter hashMap (W.Key supD W.FlagText) (\ansOld -> case ansOld of
                    (Just (W.DataText txtOld)) -> Just $ W.DataText $ txtOld <> (T.replicate (fromEnum rc_correct) $ txt <> "\n")
                    _       -> Just $ W.DataText $ T.replicate (fromEnum rc_correct) (txt <> "\n"))
            (W.AnswerInfo txt) -> W.alter hashMap (W.Key supD W.FlagText) (\ansOld -> case ansOld of
                    (Just (W.DataText txtOld)) -> Just $ W.DataText $ txtOld <> txt <> "\n"
                    _       -> Just $ W.DataText $ txt <> "\n")
            (W.AnswerButtons) -> do
                W.alter hashMap (W.Key supD W.FlagButtons) (\_ -> Just W.Empty))
    mapM_ func answers
    W.toList hashMap

sendMessages :: (Monad m, L.MonadLog m, S.MonadServer m, S.MonadTime m, W.MonadWeb m, W.OutputBotData m b) => 
                    [(W.HashMapKey b, W.HashMapData)] -> m ()
sendMessages packedMessages = do
    cpuTime <- S.getCpuTime
    sendMessagesCycle packedMessages $ cpuTime - secondsToCPU 1
    return ()

sendMessagesCycle :: (Monad m, S.MonadServer m, S.MonadTime m, W.MonadWeb m, W.OutputBotData m b) => 
                        [(W.HashMapKey b, W.HashMapData)] -> Integer -> m ()
sendMessagesCycle [] _ = return ()
sendMessagesCycle xs tStamp = do
    cpuTime <- S.getCpuTime
    let timeToSleep = secondsToCPU 1 - (cpuTime - tStamp)
    when (timeToSleep > 0) $ S.timeout $ cpuToMicro timeToSleep 
    freq <- S.getMaximumMessageFrequency
    let (now, next) = splitAt (fromEnum freq) xs
    mapM_ W.handleMessage now
    sendMessagesCycle next cpuTime

