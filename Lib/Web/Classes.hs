{-# LANGUAGE FunctionalDependencies #-}

module Web.Classes where

import qualified Data.Text as T

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (Response)
import Data.Aeson (Value, object)
import Data.Hashable (Hashable)
import qualified Network.Wreq.Session as Sess

import qualified Web.Types as Types

class Monad m => MonadWeb m where
    getSession              :: m Sess.Session
    get                     :: m ByteString
    post                    :: Value -> m (Response ByteString)
    post_simple             :: String -> m (Response ByteString)
    post_simple _ = post $ object [] 
    checkConnection         :: m () 

type ParsingError = T.Text

type Update_id = Integer

class InputBotDataUnwraped a b | a -> b where
    decodeUnwraped :: ByteString -> Either String a
    messageStatusUnwraped :: a -> (Bool, T.Text)
    messageDataUnwraped :: (Hashable b) => a -> ([ParsingError],Maybe Update_id,[Types.BotMessage b])

class (InputBotDataUnwraped a b, Monad m) => InputBotData m a b | m -> a, m -> b where
    decode :: ByteString -> m (Either String a)
    decode = return . decodeUnwraped
    messageStatus   :: a -> m (Bool, T.Text)
    messageStatus = return . messageStatusUnwraped
    messageData     :: (Hashable b) => a -> m ([ParsingError],Maybe Update_id,[Types.BotMessage b])
    messageData = return . messageDataUnwraped

class (Monad m, Hashable b) => SortingHashMap m a b | m -> a, m -> b where
    createHashMap :: m a
    alter :: a -> Types.HashMapKey b -> (Maybe Types.HashMapData -> Maybe Types.HashMapData) -> m ()
    toList :: a -> m [(Types.HashMapKey b, Types.HashMapData)]

class Monad m => OutputBotData m a | m -> a where
    handleMessage :: (Types.HashMapKey a, Types.HashMapData) -> m ()



