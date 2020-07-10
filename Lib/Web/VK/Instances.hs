{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances, QuasiQuotes #-}

module Web.VK.Instances where

import qualified Data.Text    as T
import qualified Network.Wreq as W
import qualified Data.HashTable.IO as H
import qualified Network.Wreq.Session as Sess
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.IO.Class (liftIO) 
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Control.Lens ((^?))
import Data.Aeson
import Data.Aeson.QQ

import qualified Server.Monad   as S
import qualified Env as E
import qualified Web.Types as WebT
import Config.Mode (Mode(..))
import Web.Classes (MonadWeb(..), InputBotData(..), SortingHashMap(..), OutputBotData(..))
import Web.VK.Parsing
import Control.Exception.Extends (catchLogRethrow, errorThrow)
import Logger (debug)

type HashTable k v = H.BasicHashTable k v

instance MonadWeb (ReaderT (E.Env WebT.Vkontakte) IO) where
    getSession = ReaderT E.getSession
    get = do
        uid_in <- show <$> S.getUpdateID
        sd <- T.unpack <$> S.getSupportDataString       
        let conStr = sd <> "&ts=" <> uid_in <> "&wait=25"
        s <- getSession
        r <- liftIO $ Sess.get s conStr
        debug $ T.pack $ show r
        return $ fromMaybe "" $ r ^? W.responseBody
    post json = do
        let conStr =  "https://api.vk.com/method/messages.send"
        s <- getSession
        liftIO $ Sess.post s conStr json
        -- {$server}?act=a_check&key={$key}&ts={$ts}&wait=25
    checkConnection = do
        b <- T.unpack <$> S.getBotToken
        g <- show <$> S.getGroupID 
        let conStr = "https://api.vk.com/method/groups.getLongPollServer?group_id=" <> g <> "&access_token=" <> b <> "&v=5.120"
        s <- getSession
        r <- liftIO $ Sess.get s conStr
        debug $ T.pack $ show r
        let body = r ^? W.responseBody
        when (body == Nothing) $ errorThrow $ "Cant get vkontakte long poll server credentials. " <> 
                                                    "Possibly wrong token or group ID. Exiting program."
        let json_body = fromMaybe "" body 
        let bot_data = (eitherDecode json_body :: Either String WebT.GetLongPollServer)
        case bot_data of
            (Left err) -> errorThrow $ "Cant get vkontakte long poll server credentials. Error while parsing server answer: " <> 
                                            T.pack err
            (Right (WebT.GetLongPollServer {WebT.key = key, WebT.server = server, WebT.ts = ts})) -> do
                S.setSupportDataString $ server <> "?act=a_check&key=" <> key
                S.setUpdateID ts

instance InputBotData (ReaderT (E.Env WebT.Vkontakte) IO) WebT.VKBotData WebT.VKSupportData where
    decode str = return $ eitherDecode str
    messageStatus (WebT.TelegramBotData {WebT.ok = ok, WebT.result = result}) = 
        if ok then return (ok,"") else return (False, "Telegram response - not ok, message: " <> (T.pack $ show result))
    messageData (WebT.TelegramBotData {WebT.result = result}) = return $ func result where
        func [] = ([],[],[])
        func (x:xs) = let (a,b,c) = func xs in case x of
            (WebT.UnknownMessage txt) -> (("Unknown format of telegram message, will be ignored: " <> txt):a,b,c)
            bmsg@(WebT.BotMessage _ _ upid) -> (a, upid:b, bmsg:c)

instance (HashTable (WebT.HashMapKey WebT.TelegramSupportData) WebT.HashMapData ~ hashmap) => 
    SortingHashMap (ReaderT (E.Env WebT.Telegram) IO) hashmap WebT.TelegramSupportData where
    createHashMap = liftIO $ H.new
    alter ref k f = liftIO $ H.mutate ref k ((\a -> (a,())) . f)
    toList ref = liftIO $ H.toList ref

instance OutputBotData (ReaderT (E.Env WebT.Telegram) IO) WebT.TelegramSupportData where
    handleMessage (hk, hd) = do
        askRepeatMsg <- S.getRepeateQuestion
        let buttons = [aesonQQ| {inline_keyboard: [[{text: "1", callback_data: 1},{text: "2", callback_data: 2},{text: "3", callback_data: 3},{text: "4", callback_data: 4},{text: "5", callback_data: 5}]]}  |] :: Value
        let dt = case hk of 
                (WebT.Key (WebT.TelegramSupportData chatID) WebT.FlagText)    -> case hd of
                    (WebT.DataText outTxt)     -> object ["chat_id" .= chatID, "text" .= outTxt]
                    WebT.Empty                 -> object ["chat_id" .= chatID, "text" .= ("" :: T.Text)]        
                (WebT.Key (WebT.TelegramSupportData chatID) WebT.FlagButtons) -> 
                    object ["chat_id" .= chatID, "text" .= askRepeatMsg, "reply_markup" .= buttons]
        post dt -- ignoring social network answer for now
        return ()
                