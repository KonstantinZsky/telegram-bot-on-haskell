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
import System.Random
import Data.Int

import qualified Server.Monad   as S
import qualified Env as E
import qualified Web.Types as WebT
import qualified Web.VK.Templates as Templates
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
    post_simple str = do
        let conStr =  "https://api.vk.com/method/messages.send"
        s <- getSession
        liftIO $ Sess.get s (conStr <> str)
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
                                            T.pack err <> ". Exiting program."
            (Right (WebT.GetLongPollServer {WebT.key = key, WebT.server = server, WebT.tsGP = ts})) -> do
                S.setSupportDataString $ server <> "?act=a_check&key=" <> key
                S.setUpdateID ts

instance InputBotData (ReaderT (E.Env WebT.Vkontakte) IO) WebT.VKBotData WebT.VKSupportData where
    decode str = return $ eitherDecode str
    messageStatus _ = return (True,"")
    messageData vkbd@(WebT.VKBotData {WebT.ts = tsBD, WebT.updates = updates}) = do
        ts <- catchLogRethrow ("Wrong vkontakte response, can't read value of ts: " <> (T.pack $ show vkbd)) 
            (return $ read $ T.unpack tsBD)
        return $ (\(a,b) -> (a,(Just ts),b)) $ func updates where
            func [] = ([],[])
            func (x:xs) = let (a,c) = func xs in case x of
                (WebT.UnknownMessageVK txt) -> (("Unknown format of vkontakte message, will be ignored: " <> txt):a,c)
                (WebT.VKBotMessage mt sd) -> (a, (WebT.BotMessage mt sd):c)

instance OutputBotData (ReaderT (E.Env WebT.Vkontakte) IO) WebT.VKSupportData where
    handleMessage (hk, hd) = do
        askRepeatMsg <- S.getRepeateQuestion
        b <- S.getBotToken
        g <- liftIO newStdGen
        rm <- S.getRepeateQuestion
        let random_id = fst (randomR (1, maxBound) g :: (Int64, StdGen))
        let dt = case hk of 
                (WebT.Key (WebT.VKSupportData userID) WebT.FlagText)    -> case hd of
                    (WebT.DataText outTxt)     -> 
                        "?user_id=" <> (show userID :: [Char]) <> "&random_id=" <> show random_id <> "&message=" <> T.unpack outTxt <> 
                        "&access_token=" <> T.unpack b <> "&v=5.120"
                    WebT.Empty                 -> "" -- wrong case, shouldnt be possible      
                (WebT.Key (WebT.VKSupportData userID) WebT.FlagButtons) -> 
                    "?user_id=" <> (show userID :: [Char]) <> "&random_id=" <> show random_id <> "&message=" <> T.unpack rm <> 
                    "&keyboard=" <> Templates.buttons_string <> "&access_token=" <> T.unpack b <> "&v=5.120"
        r <- post_simple dt
        --r <- post dt -- ignoring social network answer for now
        debug $ T.pack $ show r
        return ()

instance (HashTable (WebT.HashMapKey WebT.VKSupportData) WebT.HashMapData ~ hashmap) => 
    SortingHashMap (ReaderT (E.Env WebT.Vkontakte) IO) hashmap WebT.VKSupportData where
    createHashMap = liftIO $ H.new
    alter ref k f = liftIO $ H.mutate ref k ((\a -> (a,())) . f)
    toList ref = liftIO $ H.toList ref        
                