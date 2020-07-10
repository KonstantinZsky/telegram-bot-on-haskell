{-# LANGUAGE FunctionalDependencies #-}

module Web.Classes where

import qualified Data.Text as T

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (Response)
import Data.Aeson (Value)
import Data.Hashable (Hashable)
import qualified Network.Wreq.Session as Sess

import qualified Web.Types as Types

class Monad m => MonadWeb m where
    getSession              :: m Sess.Session
    get                     :: m ByteString
    post                    :: Value -> m (Response ByteString)
    checkConnection         :: m () 

type ParsingError = T.Text

type Update_id = Integer

class Monad m => InputBotData m a b | m -> a, m -> b where
    decode :: ByteString -> m (Either String a)
    messageStatus   :: a -> m (Bool, T.Text)
    messageData     :: (Hashable b) => a -> m ([ParsingError],[Update_id],[Types.BotMessage b])

class (Monad m, Hashable b) => SortingHashMap m a b | m -> a, m -> b where
    createHashMap :: m a
    alter :: a -> Types.HashMapKey b -> (Maybe Types.HashMapData -> Maybe Types.HashMapData) -> m ()
    toList :: a -> m [(Types.HashMapKey b, Types.HashMapData)]

class Monad m => OutputBotData m a | m -> a where
    handleMessage :: (Types.HashMapKey a, Types.HashMapData) -> m ()



