{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module TestInstances.Server where

import qualified Data.Text as T
import Control.Monad.State

import Server.Monad
import TestInstances.Env

instance MonadServer (State (TEnv a b)) where
    getUpdateID = get >>= (return . updateID)
    setUpdateID upid = modify $ \env -> env {updateID = upid}
    getRepeatCount = get >>= (return . repeatCount)
    setRepeatCount rc = modify $ \env -> env {repeatCount = rc}
    getSavedTimestamp = get >>= (return . cpuTimestamp)
    setSavedTimestamp = return ()
    getSupportDataString = get >>= (return . supportDataString)
    setSupportDataString ss = modify $ \env -> env {supportDataString = ss}
    getHelpMessage = get >>= (return . helpMessage)
    getRepeateQuestion = get >>= (return . repeateQuestion)
    getBotToken = get >>= (return . botToken)
    getGroupID = get >>= (return . groupID)
    getPollTimeoutMicroseconds = get >>= (return . pollTimeoutMicroseconds)
    getMaximumMessageFrequency = get >>= (return . maximumMessageFrequency)

addTestOutput :: T.Text -> State (TEnv a b) ()
addTestOutput msg = modify $ \env@(Env {testOutput = to}) -> env {testOutput = (msg:to)}

instance Monad m => MonadTime (StateT env m) where -- used for testing
    timeout s = return ()
    getTimestamp = return 0