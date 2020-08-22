module TestInstances.TestRun where

import Data.Hashable (Hashable)
import Control.Monad.State
import Data.HashMap
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

import qualified Server.Monad   as S
import qualified Logger         as L
import qualified Logger.Verbosity as L
import qualified Web            as W
import qualified Web.Types      as W
import qualified Web.Parsing    as P
import qualified Control.Exception.Extends as E
import qualified Testing.TestErrorThrow as Test
import TestInstances.Env
import TestInstances.Logger
import TestInstances.MonadError
import TestInstances.Server
import TestInstances.Web
import TestInstances.Web.Types
import Server

cycleTelegram :: State (TEnv TestTelegram HashMapTelegram) ()
cycleTelegram = do
    tIn <- testInput <$> Control.Monad.State.get
    case tIn of
        []  -> return ()
        _   -> cycle_step >> cycleTelegram    

testRunTelegram :: [B.ByteString] -> [T.Text]
testRunTelegram input = fst $ runState (do
    cycleTelegram
    testOutput <$> Control.Monad.State.get
    ) (Env  { verbosity                 = L.Info
            , updateID                  = 0
            , repeatCount               = 1
            , cpuTimestamp              = 0
            , supportDataString         = ""
            , helpMessage               = "Help message"
            , repeateQuestion           = "Repeat question"
            , botToken                  = "token" 
            , groupID                   = 0 
            , pollTimeoutMicroseconds   = 0
            , maximumMessageFrequency   = 5
            , testHashMap               = empty 
            , testOutput                = [""]
            , testInput                 = input
            } :: TEnv TestTelegram HashMapTelegram)

cycleVK :: State (TEnv TestVK HashMapVK) ()
cycleVK = do
    tIn <- testInput <$> Control.Monad.State.get
    case tIn of
        []  -> return ()
        _   -> cycle_step >> cycleVK   

testRunVK :: [B.ByteString] -> [T.Text]
testRunVK input = fst $ runState (do
    cycleVK
    testOutput <$> Control.Monad.State.get
    ) (Env  { verbosity                 = L.Info
            , updateID                  = 0
            , repeatCount               = 1
            , cpuTimestamp              = 0
            , supportDataString         = ""
            , helpMessage               = "Help message"
            , repeateQuestion           = "Repeat question"
            , botToken                  = "token" 
            , groupID                   = 0 
            , pollTimeoutMicroseconds   = 0
            , maximumMessageFrequency   = 5
            , testHashMap               = empty 
            , testOutput                = [""]
            , testInput                 = input
            } :: TEnv TestVK HashMapVK)