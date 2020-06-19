module Server.Monad where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Text              as T
import Control.Monad.IO.Class (MonadIO, liftIO) 
import Control.Concurrent (threadDelay)

import qualified Env as E

class Monad m => MonadServer m where
    getUpdateID                 :: m Integer
    setUpdateID                 :: (Integer -> m ())
    getRepeatCount              :: m Integer
    setRepeatCount              :: (Integer -> m ())
    getHelpMessage              :: m T.Text
    getRepeateQuestion          :: m T.Text
    getBotToken                 :: m T.Text
    getPollTimeoutMicroseconds  :: m Integer   

instance E.HasData env m => MonadServer (ReaderT env m) where
    getUpdateID = ReaderT E.getUpdateID    
    setUpdateID x = ReaderT $ \env -> E.setUpdateID env x
    getRepeatCount = ReaderT E.getRepeatCount
    setRepeatCount x = ReaderT $ \env ->  E.setRepeatCount env x
    getHelpMessage = ReaderT E.getHelpMessage
    getRepeateQuestion = ReaderT E.getRepeateQuestion
    getBotToken = ReaderT E.getBotToken
    getPollTimeoutMicroseconds = ReaderT E.getPollTimeoutMicroseconds

class Monad m => MonadTimeout m where
    timeout :: Int -> m () 

instance (MonadIO m, Monad m) => MonadTimeout (ReaderT env m) where
    timeout = liftIO . threadDelay

instance Monad m => MonadTimeout (StateT env m) where -- used for testing
    timeout s = return ()