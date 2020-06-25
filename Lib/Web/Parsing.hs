module Web.Parsing where

import Data.ByteString.Lazy (ByteString)
import Data.Aeson (eitherDecode)
import qualified Data.Text as T

import Web.Telegram.Parsing
import Env (Env, HasMode(..))
import Config.Mode (Mode(..))
import qualified Web.Types  as W
import qualified Web        as W
import qualified Logger     as L
import qualified Control.Exception.Extends as E

parseAnswer :: (Monad m, W.MonadWeb m, L.MonadLog m, E.MonadError m) => ByteString -> m W.BotData
parseAnswer input = do
    mode <- W.getMode  
    let bot_data = case mode of
                        TG -> (eitherDecode input :: Either String W.TelegramBotData)
                        VK -> undefined -- not implemented yet
    case bot_data of
        (Left err)  -> E.errorThrow $ "Impossible parsing error while processing bot answer: " <> (T.pack $ show err)   
        (Right btd) -> return $ W.BotData btd