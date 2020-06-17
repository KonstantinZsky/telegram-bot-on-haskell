module Server.Monad where

import Control.Monad.Trans.Reader
import qualified Data.Text              as T

import qualified Env as E

class Monad m => MonadServer m where
    getUpdateID                 :: m Integer
    setUpdateID                 :: (Integer -> m ())
    getRepeatCount              :: m Integer
    setRepeatCount              :: (Integer -> m ())
    getHelpMessage              :: m T.Text
    getrepeateQuestion          :: m T.Text
    getbotToken                 :: m T.Text
    getPollTimeoutMicroseconds  :: m Integer   

instance E.HasData env m => MonadServer (ReaderT env m) where
    getUpdateID = ReaderT E.getUpdateID    
    setUpdateID x = ReaderT $ \env -> E.setUpdateID env x
    getRepeatCount = ReaderT E.getRepeatCount
    setRepeatCount x = ReaderT $ \env ->  E.setRepeatCount env x
    getHelpMessage = ReaderT E.getHelpMessage
    getrepeateQuestion = ReaderT E.getrepeateQuestion
    getbotToken = ReaderT E.getbotToken
    getPollTimeoutMicroseconds = ReaderT E.getPollTimeoutMicroseconds