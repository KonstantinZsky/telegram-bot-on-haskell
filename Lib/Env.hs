{-# LANGUAGE MultiParamTypeClasses #-}

module Env
    ( Env   (..)
    , HasLog (..)
    , HasData (..)
    , HasMode (..)
    ) where

import qualified Data.Text as T
import Data.IORef
import Prelude hiding (error)

import Logger.Verbosity
import Config.Mode (Mode(..))

data Env = Env 
    { mode                      :: !(Mode)
    , envLog                    :: !(T.Text -> IO ())
    , verbosity                 :: !Verbosity
    , updateID                  :: !(IORef Integer)
    , repeatCount               :: !(IORef Integer)
    , helpMessage               :: !T.Text
    , repeateQuestion           :: !T.Text
    , botToken                  :: !T.Text  
    , pollTimeoutMicroseconds   :: !Integer
    }

class Monad m => HasLog env m where 
    getLog          :: env -> (T.Text -> m ())
    getVerbosity    :: env -> m Verbosity

instance HasLog Env IO where
    getLog          = envLog
    getVerbosity    = return . verbosity

class Monad m => HasData env m where
    getUpdateID                 :: env -> m Integer
    setUpdateID                 :: env -> (Integer -> m ())
    getRepeatCount              :: env -> m Integer
    setRepeatCount              :: env -> (Integer -> m ())
    getHelpMessage              :: env -> m T.Text
    getrepeateQuestion          :: env -> m T.Text
    getbotToken                 :: env -> m T.Text
    getPollTimeoutMicroseconds  :: env -> m Integer

instance HasData Env IO where
    getUpdateID                 = readIORef . updateID
    setUpdateID                 = writeIORef . updateID
    getRepeatCount              = readIORef . repeatCount
    setRepeatCount              = writeIORef . repeatCount
    getHelpMessage              = return . helpMessage 
    getrepeateQuestion          = return . repeateQuestion
    getbotToken                 = return . botToken
    getPollTimeoutMicroseconds  = return . pollTimeoutMicroseconds

class Monad m => HasMode env m where 
    getMode          :: env -> m Mode

instance HasMode Env IO where
    getMode                     = return . mode







