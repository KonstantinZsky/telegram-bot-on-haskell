{-# LANGUAGE MultiParamTypeClasses #-}

module Env
    ( Env   (..)
    , HasLog (..)
    , HasData (..)
    , HasMode (..)
    , HasSortingHashTable (..)
    ) where

import qualified Data.Text as T
import Data.IORef
import Prelude hiding (error)
import qualified Network.Wreq.Session as Sess
import qualified Data.HashTable.IO as H

import qualified Web.Types as W
import Logger.Verbosity
import Config.Mode (Mode(..))

type HashTable k v = H.BasicHashTable k v

data Env = Env 
    { mode                      :: !(Mode)
    , session                   :: !(Sess.Session)
    , envLog                    :: !(T.Text -> IO ())
    , verbosity                 :: !Verbosity
    , updateID                  :: !(IORef Integer)
    , repeatCount               :: !(IORef Integer)
    , helpMessage               :: !T.Text
    , repeateQuestion           :: !T.Text
    , botToken                  :: !T.Text  
    , pollTimeoutMicroseconds   :: !Integer
    , sortingHashTable          :: !(HashTable W.SupportData W.AnswerType)
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
    getRepeateQuestion          :: env -> m T.Text
    getBotToken                 :: env -> m T.Text
    getPollTimeoutMicroseconds  :: env -> m Integer

instance HasData Env IO where
    getUpdateID                 = readIORef . updateID
    setUpdateID                 = writeIORef . updateID
    getRepeatCount              = readIORef . repeatCount
    setRepeatCount              = writeIORef . repeatCount
    getHelpMessage              = return . helpMessage 
    getRepeateQuestion          = return . repeateQuestion
    getBotToken                 = return . botToken
    getPollTimeoutMicroseconds  = return . pollTimeoutMicroseconds

class Monad m => HasMode env m where 
    getMode             :: env -> m Mode
    getSession          :: env -> m Sess.Session

instance HasMode Env IO where
    getMode                     = return . mode
    getSession                  = return . session

class Monad m => HasSortingHashTable env m where
    emptyHashTable :: env -> m ()
    alter :: env -> W.SupportData -> ((Maybe W.AnswerType -> Maybe W.AnswerType) -> m ())
    toList :: env -> m [(W.SupportData, W.AnswerType)]    

instance HasSortingHashTable Env IO where
    emptyHashTable = (\h -> (H.toList h) >>= (mapM_ $ \(k,_) -> H.delete h k)) . sortingHashTable
    alter = (\h -> \k f -> H.mutate h k ((\a -> (a,())) . f)) . sortingHashTable
    toList = H.toList . sortingHashTable