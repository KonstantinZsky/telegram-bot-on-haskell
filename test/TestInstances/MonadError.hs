{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module TestInstances.MonadError where

import Control.Monad.State
import qualified Data.Text          as T

import Control.Exception.Extends
import TestInstances.Env
import TestInstances.Web.Types
import Web.Types
import Testing.TestErrorThrow

instance MonadError (State (TEnv mode a)) where 
    catchLogRethrow txt action = do
        out <- action
        return out
    errorThrow msg = return $ testErrorThrow msg