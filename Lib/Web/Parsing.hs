module Web.Parsing where

import Data.ByteString.Lazy (ByteString)
import Data.Aeson (eitherDecode)
import Control.Monad (guard)
import qualified Data.Text as T

import Web.Telegram.Parsing
import Config.Mode (Mode(..))
import qualified Web.Types  as W
import qualified Web        as W
import qualified Logger     as L
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
prepareOutput :: (Monad m, E.MonadError m, L.MonadLog m) => W.BotData -> [(W.MessageType, W.AnswerType)] -> m [W.BotAnswer]
prepareOutput (W.VkBotData) _ = undefined -- not implemented yet
prepareOutput (W.BotData (W.TelegramBotData {W.ok = False, W.result = result})) _ = 
    E.errorThrow $ "Telegram response - not ok, message: " <> (T.pack $ show result)
prepareOutput (W.BotData (W.TelegramBotData {W.result = []})) _ = return []
prepareOutput (W.BotData (W.TelegramBotData {W.result = result})) processing = do
    let warnings = [ x | x@(W.UnknownMessage _) <- result ]
        upids = [ update_id | (W.TelegramBotMessage {W.update_id = update_id}) <- result ]
        answers = do
            W.TelegramBotMessage {W.messageType = messageType, W.chat_id = chat_id, W.update_id = update_id} <- result
            (m,a) <- processing
            guard (m == messageType)
            return $ W.BotAnswer a $ W.TelegramSupportData chat_id
    mapM_ (\(W.UnknownMessage txt) -> L.warning $ "Unknown format of telegram message, will be ignored: " <> txt) warnings
    return answers -- GET UPID !

