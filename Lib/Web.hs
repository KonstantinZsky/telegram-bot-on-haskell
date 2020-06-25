{-# LANGUAGE FlexibleInstances #-}

module Web (MonadWeb (getMode,get,post)) where

import Control.Monad.Trans.Reader (ReaderT(..))
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (Response)
import Control.Monad.IO.Class (liftIO) 
import Data.Maybe (fromMaybe)
import Data.Aeson (Value)
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as Sess
import Control.Lens ((^?))
import qualified Data.Text      as T

import qualified Env as E
import Config.Mode (Mode(..))
import qualified Server.Monad   as S

class Monad m => MonadWeb m where
    getMode                 :: m Mode
    getSession              :: m Sess.Session
    get                     :: m ByteString
    post                    :: Value -> m (Response ByteString)
 
instance MonadWeb (ReaderT E.Env IO) where
    getSession = ReaderT E.getSession
    getMode = ReaderT E.getMode
    get = do
        mode <- getMode
        uid_in <- S.getUpdateID
        b <- T.unpack <$> S.getBotToken        
        let conStr = case mode of
                        TG -> if uid_in == (-1) then "https://api.telegram.org/bot" <> b <> "/getUpdates"
                                else "https://api.telegram.org/bot" <> b <> "/getUpdates?offset=" <> (show uid_in)
                        VK -> undefined -- not implemented yet
        s <- getSession
        r <- liftIO $ Sess.get s conStr
        return $ fromMaybe "" $ r ^? W.responseBody
    post json = do
        mode <- getMode
        b <- T.unpack <$> S.getBotToken
        let conStr = case mode of
                        TG -> "https://api.telegram.org/bot" <> b <> "/sendMessage"
                        VK -> undefined -- not implemented yet
        s <- getSession
        liftIO $ Sess.post s conStr json