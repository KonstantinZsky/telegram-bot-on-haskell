module Env.Init where

import qualified Data.IORef     as IORef
import qualified System.IO      as System
import qualified Data.Text.IO       as T
import qualified Data.HashTable.IO  as H
import qualified Network.Wreq.Session as Sess
import System.IO (hFlush)
import System.CPUTime (getCPUTime)

import qualified Config as C
import qualified Env    as E   
import Data.Time.Extended (getCurrentTime)
import Control.Exception.Extends (catchLogRethrow)

initEnv :: System.Handle -> C.Config -> IO (E.Env a)
initEnv handle C.Config { C.cMode = m 
                        , C.cLogVerbosity = v
                        , C.cHelpMessage = h
                        , C.cRepeatQuestion = rq
                        , C.cRepeatCount = rc
                        , C.cPollTimeoutMicroseconds = p
                        , C.cMaximumMessageFrequency = f
                        , C.cBotToken = b
                        , C.cGroupID = gi} = do
    uID         <- IORef.newIORef (-1)
    rc_ref      <- IORef.newIORef rc
    sd          <- IORef.newIORef ""
    sess        <- Sess.newSession
    ct          <- getCPUTime
    timeStamp   <- IORef.newIORef $ ct
    return E.Env    { E.session                     = sess
                    , E.envLog                      =
                        -- may be change error to warning? 
                        \str -> getCurrentTime >>= \t -> catchLogRethrow "Error while writing a log file. Exiting program. "
                                                               $ T.hPutStrLn handle (t <> " " <> str) >> hFlush handle
                    , E.verbosity                   = v
                    , E.updateID                    = uID
                    , E.repeatCount                 = rc_ref
                    , E.cpuTimestamp                = timeStamp
                    , E.supportDataString           = sd
                    , E.helpMessage                 = h
                    , E.repeateQuestion             = rq
                    , E.botToken                    = b
                    , E.groupID                     = gi
                    , E.pollTimeoutMicroseconds     = p
                    , E.maximumMessageFrequency     = f}