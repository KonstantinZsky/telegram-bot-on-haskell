{-# LANGUAGE MultiParamTypeClasses #-}

module Env
    ( Env   (..)
    , HasLog (..)
    , HasData (..)
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
    { session                   :: !(Sess.Session)
    , envLog                    :: !(T.Text -> IO ())
    , verbosity                 :: !Verbosity
    , updateID                  :: !(IORef Integer)
    , repeatCount               :: !(IORef Integer)
    , cpuTimestamp              :: !(IORef Integer)
    , supportDataString         :: !(IORef T.Text)
    , helpMessage               :: !T.Text
    , repeateQuestion           :: !T.Text
    , botToken                  :: !T.Text 
    , groupID                   :: !Integer 
    , pollTimeoutMicroseconds   :: !Integer
    , maximumMessageFrequency   :: !Integer
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
    getSupportDataString        :: env -> m T.Text
    setSupportDataString        :: env -> T.Text -> m ()
    getHelpMessage              :: env -> m T.Text
    getRepeateQuestion          :: env -> m T.Text
    getBotToken                 :: env -> m T.Text
    getGroupID                  :: env -> m Integer
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
    getSupportDataString        = readIORef . supportDataString
    setSupportDataString        = writeIORef . supportDataString 
    getHelpMessage              = return . helpMessage 
    getRepeateQuestion env      = do
        rc <- getRepeatCount env
        let rq = repeateQuestion env
        return $ "Current repeat count: " <> (T.pack $ show rc) <> "\n" <> rq
    getBotToken                 = return . botToken
    getGroupID                  = return . groupID
    getPollTimeoutMicroseconds  = return . pollTimeoutMicroseconds
    getMaximumMessageFrequency  = return . maximumMessageFrequency
    getSession                  = return . session
