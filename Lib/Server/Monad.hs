module Server.Monad where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Text              as T
import Control.Monad.IO.Class (MonadIO, liftIO) 
import Control.Concurrent (threadDelay)

import qualified Data.Time.Extended as Time
import qualified Env as E
import qualified Web.Types as W

class Monad m => MonadServer m where
    getUpdateID                 :: m Integer
    setUpdateID                 :: (Integer -> m ())
    getRepeatCount              :: m Integer
    setRepeatCount              :: (Integer -> m ())
    getSavedTimestamp           :: m Double
    setSavedTimestamp           :: m () 
    getSupportDataString        :: m T.Text
    setSupportDataString        :: (T.Text -> m ())      
    getHelpMessage              :: m T.Text
    getRepeateQuestion          :: m T.Text
    getBotToken                 :: m T.Text
    getGroupID                  :: m Integer
    getPollTimeoutMicroseconds  :: m Integer
    getMaximumMessageFrequency  :: m Integer   

instance E.HasData env m => MonadServer (ReaderT env m) where
    getUpdateID = ReaderT E.getUpdateID    
    setUpdateID x = ReaderT $ \env -> E.setUpdateID env x
    getRepeatCount = ReaderT E.getRepeatCount
    setRepeatCount x = ReaderT $ \env -> E.setRepeatCount env x
    getSavedTimestamp = ReaderT E.getSavedTimestamp
    setSavedTimestamp = ReaderT E.setSavedTimestamp 
    getSupportDataString = ReaderT E.getSupportDataString
    setSupportDataString x = ReaderT $ \env -> E.setSupportDataString env x 
    getHelpMessage = ReaderT E.getHelpMessage
    getRepeateQuestion = ReaderT E.getRepeateQuestion
    getBotToken = ReaderT E.getBotToken
    getGroupID = ReaderT E.getGroupID
    getPollTimeoutMicroseconds = ReaderT E.getPollTimeoutMicroseconds
    getMaximumMessageFrequency = ReaderT E.getMaximumMessageFrequency

class Monad m => MonadTime m where
    timeout :: Int -> m ()
    getTimestamp :: m Double 

instance (MonadIO m, Monad m) => MonadTime (ReaderT env m) where
    timeout t = liftIO $ threadDelay t
    getTimestamp = liftIO $ Time.getTimestamp