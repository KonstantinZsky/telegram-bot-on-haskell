{-# LANGUAGE FlexibleInstances #-}

module Testing.TestErrorThrow where

import qualified Data.Text          as T
import Web.Types

class TestErrorThrow a where
    testErrorThrow :: T.Text -> a

instance TestErrorThrow () where
    testErrorThrow _ = ()

instance TestErrorThrow TelegramBotData where
    testErrorThrow msg = TelegramBotData {ok = True, result = [UnknownMessageTG msg]}

instance TestErrorThrow VKBotData where
    testErrorThrow msg = VKBotData {ts = 0, updates = [UnknownMessageVK msg]}

instance TestErrorThrow [BotAnswer TelegramSupportData] where
    testErrorThrow msg = [BotAnswer (AnswerInfo msg) (TelegramSupportData 0)]   

instance TestErrorThrow [BotAnswer VKSupportData] where
    testErrorThrow msg = [BotAnswer (AnswerInfo msg) (VKSupportData 0)]