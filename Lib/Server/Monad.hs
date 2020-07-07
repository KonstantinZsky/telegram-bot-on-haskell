module Server.Monad where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Text              as T
import Control.Monad.IO.Class (MonadIO, liftIO) 
import Control.Concurrent (threadDelay)
import System.CPUTime (getCPUTime)

import qualified Env as E
import qualified Web.Types as W

class Monad m => MonadServer m where
    getUpdateID                 :: m Integer
    setUpdateID                 :: (Integer -> m ())
    getRepeatCount              :: m Integer
    setRepeatCount              :: (Integer -> m ())
    getCpuTimestamp             :: m Integer
    setCpuTimestamp             :: m ()    
    getHelpMessage              :: m T.Text
    getRepeateQuestion          :: m T.Text
    getBotToken                 :: m T.Text
    getPollTimeoutMicroseconds  :: m Integer
    getMaximumMessageFrequency  :: m Integer   

instance E.HasData env m => MonadServer (ReaderT env m) where
    getUpdateID = ReaderT E.getUpdateID    
    setUpdateID x = ReaderT $ \env -> E.setUpdateID env x
    getRepeatCount = ReaderT E.getRepeatCount
    setRepeatCount x = ReaderT $ \env ->  E.setRepeatCount env x
    getCpuTimestamp = ReaderT E.getCpuTimestamp
    setCpuTimestamp = ReaderT E.setCpuTimestamp  
    getHelpMessage = ReaderT E.getHelpMessage
    getRepeateQuestion = ReaderT E.getRepeateQuestion
    getBotToken = ReaderT E.getBotToken
    getPollTimeoutMicroseconds = ReaderT E.getPollTimeoutMicroseconds
    getMaximumMessageFrequency = ReaderT E.getMaximumMessageFrequency

class Monad m => MonadTime m where
    timeout :: Int -> m ()
    getCpuTime :: m Integer 

instance (MonadIO m, Monad m) => MonadTime (ReaderT env m) where
    timeout = liftIO . threadDelay
    getCpuTime = liftIO getCPUTime

instance Monad m => MonadTime (StateT env m) where -- used for testing
    timeout s = return ()
    getCpuTime = return 0

{-
-- for packing output messagies
class Monad m => MonadSortingHashTable m where
    emptyHashTable :: m ()
    alter :: W.HashMapKey -> ((Maybe W.HashMapData -> Maybe W.HashMapData) -> m ())
    toList :: m [(W.HashMapKey, W.HashMapData)]       

instance E.HasSortingHashTable env m => MonadSortingHashTable (ReaderT env m) where
    emptyHashTable = ReaderT E.emptyHashTable
    alter k f = ReaderT $ \env -> E.alter env k f
    toList = ReaderT E.toList -}