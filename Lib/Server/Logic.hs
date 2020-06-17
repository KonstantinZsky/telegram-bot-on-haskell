module Server.Logic where

import System.IO
import Data.IORef
import Data.Aeson
import Control.Monad (forever)
import Control.Monad.Trans.Reader
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO) 
import qualified Data.Text.IO           as T
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import Prelude hiding (error)

import Data.Time.Extended (getCurrentTime)
import Web.Telegram.HTTP (handleMessages)
import qualified Server.Monad as S
import Web.Telegram.Parsing
import Config.Logic
import qualified Env as E
import qualified Logging.Logger as L
import Data.Time.Extended (getCurrentTime)
import qualified Server.Monad as M 
import qualified Web.Monad as W

runServer :: ReaderT E.Env IO ()
runServer = do
    pollTimeout <- S.getPollTimeoutMicroseconds
    forever $ do
        liftIO $ threadDelay $ fromEnum pollTimeout
        cycle_step

--cycle_step :: MonadServer m => m ()
cycle_step :: (S.MonadServer m, W.MonadWeb m, MonadIO m) => m ()
cycle_step = do
    jsonBody <- W.get
    let bot_data = eitherDecode jsonBody
    case bot_data of
        (Right bdt) -> do
            handleMessages bdt
        (Left err) -> liftIO $ putStrLn err ---}



initEnv :: Handle -> Config -> IO E.Env
initEnv handle Config   { cMode = m 
                        , cLogVerbosity = v
                        , cHelpMessage = h
                        , cRepeatQuestion = rq
                        , cRepeatCount = rc
                        , cPollTimeoutMicroseconds = p
                        , cBotToken = b} = do
    uID     <- newIORef (-1)
    rc_ref  <- newIORef rc
    return E.Env    { E.mode                      = m
                    , E.envLog                    = \str -> getCurrentTime >>= \t -> T.hPutStrLn handle $ t <> " " <> str
                    , E.verbosity                 = v
                    , E.updateID                  = uID
                    , E.repeatCount               = rc_ref
                    , E.helpMessage               = h
                    , E.repeateQuestion           = rq
                    , E.botToken                  = b
                    , E.pollTimeoutMicroseconds   = p }