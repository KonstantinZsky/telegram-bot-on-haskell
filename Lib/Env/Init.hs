module Env.Init where

import qualified Data.IORef     as IORef
import qualified System.IO      as System
import qualified Data.Text.IO   as T
import qualified Network.Wreq.Session as Sess

import qualified Config as C
import qualified Env    as E   
import Data.Time.Extended (getCurrentTime)
import Control.Exception.Extends (catchLogRethrow)

initEnv :: System.Handle -> C.Config -> IO E.Env
initEnv handle C.Config { C.cMode = m 
                        , C.cLogVerbosity = v
                        , C.cHelpMessage = h
                        , C.cRepeatQuestion = rq
                        , C.cRepeatCount = rc
                        , C.cPollTimeoutMicroseconds = p
                        , C.cBotToken = b} = do
    uID     <- IORef.newIORef (-1)
    rc_ref  <- IORef.newIORef rc
    sess    <- Sess.newSession
    return E.Env    { E.mode                      = m
                    , E.session                   = sess
                    , E.envLog                    =
                        -- may be change error to warning? 
                        \str -> getCurrentTime >>= \t -> catchLogRethrow "Error while writing a log file. Exiting program. "
                                                                (T.hPutStrLn handle $ t <> " " <> str)
                    , E.verbosity                 = v
                    , E.updateID                  = uID
                    , E.repeatCount               = rc_ref
                    , E.helpMessage               = h
                    , E.repeateQuestion           = rq
                    , E.botToken                  = b
                    , E.pollTimeoutMicroseconds   = p }