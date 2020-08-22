{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module TestInstances.Logger where

import Control.Monad.State
import qualified Data.Text as T
import Prelude hiding (log, error)

import Logger
import Logger.Verbosity
import TestInstances.Env

instance MonadLog (State (TEnv mode a)) where
    pushLogMessage msg = modify $ \env@(Env {testOutput = testOutput}) -> env {testOutput = (msg:testOutput)}
    debug   msg = instanceHelper Debug      msg
    info    msg = instanceHelper Info       msg
    warning msg = instanceHelper Warning    msg
    error   msg = instanceHelper Error      msg   

instanceHelper :: Verbosity -> T.Text -> State (TEnv mode a) ()
instanceHelper v' msg = do
    v <- verbosity <$> get
    if (v > v') then return () else log v' msg