module Server where

import System.IO
import Data.IORef
import Data.Aeson
import Control.Monad (forever)
import Control.Monad.Trans.Reader
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO) 
import qualified Data.Text.IO   as T
import qualified Data.Text      as T
import qualified Data.Text.Encoding as T
import Prelude hiding (error)

import Web.Telegram.HTTP (getTeleramUpdate, handleMessages)
import Web.Telegram.Parsing
import Config
import Env
import qualified Logger as L


initEnv :: Handle -> Config -> IO Env
initEnv handle Config   { cLogVerbosity = v
                        , cHelpMessage = h
                        , cRepeatQuestion = rq
                        , cRepeatCount = rc
                        , cPollTimeoutMicroseconds = p
                        , cBotToken = b} = do
    uID     <- newIORef (-1)
    rc_ref  <- newIORef rc
    return Env  { envLog                    = \str -> T.hPutStrLn handle str
                , debug                     = L.makeLoggingFunction v L.Debug
                , info                      = L.makeLoggingFunction v L.Info
                , warning                   = L.makeLoggingFunction v L.Warning
                , error                     = L.makeLoggingFunction v L.Error
                , updateID                  = uID
                , repeatCount               = rc_ref
                , helpMessage               = h
                , repeateQuestion           = rq
                , botToken                  = b
                , pollTimeoutMicroseconds   = p }

runServer :: ReaderT Env IO ()
runServer = do
    pollTimeout <- pollTimeoutMicroseconds <$> ask
    runForever $ fromEnum pollTimeout


runForever :: Int -> ReaderT Env IO ()
runForever delay = forever $ do
    liftIO $ threadDelay delay
    cycle_step

cycle_step :: ReaderT Env IO ()
cycle_step = do
    uid_inRaw <- updateID <$> ask
    uid_in <- liftIO $ readIORef uid_inRaw
    b <- botToken <$> ask
    let connecStr = if uid_in == (-1) then "https://api.telegram.org/bot" <> b <> "/getUpdates"
        else "https://api.telegram.org/bot" <> b <> "/getUpdates?offset=" <> (T.pack $ show uid_in)
    jsonBody <- liftIO $ getTeleramUpdate connecStr
    let bot_data = eitherDecode jsonBody
    case bot_data of
        (Right bdt) -> do
            handleMessages bdt
        (Left err) -> liftIO $ putStrLn err