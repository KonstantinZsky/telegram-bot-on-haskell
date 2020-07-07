{-# LANGUAGE MultiParamTypeClasses #-}

module Env
    ( Env   (..)
    , HasLog (..)
    , HasData (..)
    --, HasMode (..)
    --, HasSortingHashTable (..)
    ) where

import qualified Data.Text as T
import Data.IORef
import System.CPUTime (getCPUTime)
import Prelude hiding (error)
import qualified Network.Wreq.Session as Sess
import qualified Data.HashTable.IO as H

import qualified Web.Types as W
import Logger.Verbosity
import Config.Mode (Mode(..))

type HashTable k v = H.BasicHashTable k v

data Env mode = Env
    { --mode                      :: !(Mode),
    session                   :: !(Sess.Session)
    , envLog                    :: !(T.Text -> IO ())
    , verbosity                 :: !Verbosity
    , updateID                  :: !(IORef Integer)
    , repeatCount               :: !(IORef Integer)
    , cpuTimestamp              :: !(IORef Integer)
    , helpMessage               :: !T.Text
    , repeateQuestion           :: !T.Text
    , botToken                  :: !T.Text  
    , pollTimeoutMicroseconds   :: !Integer
    , maximumMessageFrequency   :: !Integer
    --, sortingHashTable          :: !(HashTable W.HashMapKey W.HashMapData)
    }

class Monad m => HasLog env m where 
    getLog          :: env -> (T.Text -> m ())
    getVerbosity    :: env -> m Verbosity

instance HasLog (Env a) IO where
    getLog          = envLog
    getVerbosity    = return . verbosity

class Monad m => HasData env m where
    getUpdateID                 :: env -> m Integer
    setUpdateID                 :: env -> Integer -> m ()
    getRepeatCount              :: env -> m Integer
    setRepeatCount              :: env -> Integer -> m ()
    getCpuTimestamp             :: env -> m Integer
    setCpuTimestamp             :: env -> m ()
    getHelpMessage              :: env -> m T.Text
    getRepeateQuestion          :: env -> m T.Text
    getBotToken                 :: env -> m T.Text
    getPollTimeoutMicroseconds  :: env -> m Integer
    getMaximumMessageFrequency  :: env -> m Integer
    getSession                  :: env -> m Sess.Session

instance HasData (Env a) IO where
    getUpdateID                 = readIORef . updateID
    setUpdateID                 = writeIORef . updateID
    getRepeatCount              = readIORef . repeatCount
    setRepeatCount              = writeIORef . repeatCount
    getCpuTimestamp             = readIORef . cpuTimestamp
    setCpuTimestamp env         = do
        t <- getCPUTime
        writeIORef (cpuTimestamp env) t   
    getHelpMessage              = return . helpMessage 
    getRepeateQuestion          = return . repeateQuestion
    getBotToken                 = return . botToken
    getPollTimeoutMicroseconds  = return . pollTimeoutMicroseconds
    getMaximumMessageFrequency  = return . maximumMessageFrequency
    getSession                  = return . session

{-
class Monad m => HasMode env m where 
    getMode             :: env -> m Mode


instance HasMode (Env a) IO where
    getMode                     = return . mode



class Monad m => HasSortingHashTable env m where
    emptyHashTable :: env -> m ()
    alter :: env -> W.HashMapKey -> (Maybe W.HashMapData -> Maybe W.HashMapData) -> m ()
    toList :: env -> m [(W.HashMapKey, W.HashMapData)]    

instance HasSortingHashTable (Env a) IO where
    emptyHashTable = (\h -> (H.toList h) >>= (mapM_ $ \(k,_) -> H.delete h k)) . sortingHashTable
    alter = (\h -> \k f -> H.mutate h k ((\a -> (a,())) . f)) . sortingHashTable
    toList = H.toList . sortingHashTable -}