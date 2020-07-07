{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Control.Exception.Extends (MonadError(..)) where

import Control.Monad.IO.Class (liftIO) 
import qualified Data.Text          as T
import qualified Control.Exception  as E
import Control.Monad.Trans.Reader

import qualified Logger             as L
import Env

class Monad m => MonadError m where 
    catchLogRethrow     :: T.Text -> m a -> m a
    errorThrow          :: T.Text -> m a

instance MonadError (ReaderT (Env a) IO) where -- logging with error text 
    catchLogRethrow msg action = ReaderT $ \r -> E.catch (runReaderT action r) (\err -> runReaderT (
        (L.error $ msg <> (T.pack $ show err)) >> (liftIO $ E.throwIO (err :: E.SomeException))) r)
    errorThrow msg = L.error msg >> (liftIO $ E.throwIO TelegramBotException)

{- I dont know how to say that "a" from here is the same as from the instance
errorHandler :: E.SomeException -> T.Text -> ReaderT (Env a) IO a
errorHandler err msg = do
    L.error $ msg <> (T.pack $ show err)
    liftIO $ E.throwIO err -}

data TelegramBotException = TelegramBotException
    deriving Show

instance E.Exception TelegramBotException

instance MonadError IO where -- logging without error text
    catchLogRethrow msg action = E.catch action $ rethrowErrorIO msg
    errorThrow msg = L.error msg >> E.throwIO TelegramBotException

rethrowErrorIO ::  T.Text -> E.SomeException -> IO a
rethrowErrorIO msg err = L.error msg >> E.throwIO err





