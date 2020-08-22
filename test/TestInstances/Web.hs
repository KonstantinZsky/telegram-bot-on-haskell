{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module TestInstances.Web where

import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (Response(..))
import qualified Data.Text as T
import Data.HashMap

import Server.Monad
import Web.Classes
import Web.Telegram.Instances
import Web.VK.Instances
import TestInstances.Env
import TestInstances.Server
import TestInstances.Web.Types
import qualified Web.Types as WebT

type HashMapTelegram = (Map (WebT.HashMapKey WebT.TelegramSupportData) WebT.HashMapData)

type HashMapVK = (Map (WebT.HashMapKey WebT.VKSupportData) WebT.HashMapData)

instance MonadWeb (State (TEnv mode a)) where
    getSession              = undefined     -- will never be used
    get                     = do
        tIn <- testInput <$> Control.Monad.State.get
        case tIn of
            []      -> return "Test input ended"
            (x:xs)  -> do
                modify $ \env -> env {testInput = xs}
                return x    
    post val                = undefined     -- will never be used
    post_simple _           = undefined     -- will never be used
    checkConnection         = return () 

instance InputBotData (State (TEnv TestTelegram HashMapTelegram)) WebT.TelegramBotData WebT.TelegramSupportData

instance InputBotData (State (TEnv TestVK HashMapVK)) WebT.VKBotData WebT.VKSupportData

instance OutputBotData (State (TEnv TestTelegram HashMapTelegram)) WebT.TelegramSupportData where
    handleMessage = addTestOutput . T.pack . show

instance OutputBotData (State (TEnv TestVK HashMapVK)) WebT.VKSupportData where
    handleMessage = addTestOutput . T.pack . show

instance SortingHashMap (State (TEnv TestTelegram HashMapTelegram)) HashMapTelegram WebT.TelegramSupportData where
    createHashMap = do
        let hmt = empty
        modify $ \env -> env {testHashMap = hmt}
        return hmt
    alter _ k f = modify $ \env@(Env {testHashMap = hm}) -> env {testHashMap = (Data.HashMap.alter f k hm)}
    toList _ = Control.Monad.State.get >>= (return . Data.HashMap.toList . testHashMap)

instance SortingHashMap (State (TEnv TestVK HashMapVK)) HashMapVK WebT.VKSupportData where
    createHashMap = do
        let hmt = empty
        modify $ \env -> env {testHashMap = hmt}
        return hmt
    alter _ k f = modify $ \env@(Env {testHashMap = hm}) -> env {testHashMap = (Data.HashMap.alter f k hm)}
    toList _ = Control.Monad.State.get >>= (return . Data.HashMap.toList . testHashMap)