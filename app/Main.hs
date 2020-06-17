module Main where

import qualified Data.Text      as T
import qualified System.Exit    as E
import Control.Monad (when)
import Control.Exception (bracket)
import System.IO
import Control.Monad.Trans.Reader

import qualified Config.Logic   as C
import qualified Files          as F
import qualified Server.Logic   as S
import qualified Env            as E
--import Logging.Logger (MonadLog)
import Concole (askUser)
import Web.Telegram.HTTP (checkTelegramConnection)

import Prelude hiding (error)

main :: IO ()
main = do
    F.checkForConfig "test.cfg"
    cfg <- F.logFileErrors $ C.loadConfig "test.cfg"
    putStrLn $ show cfg
    when (C.cBotToken cfg == "") $ do
        askUser "Bot token is not specifed. You must write it in config manually. (Put any char to quit program)" (return ()) (return ())
        E.exitSuccess
    checkTelegramConnection $ C.cBotToken cfg
    F.checkForFile "Log file not found. Creating " "log.txt" ""
    bracket
        (openFile "log.txt" WriteMode)
        (hClose)
        (\fileHandle -> do
            env <- S.initEnv fileHandle cfg
            runReaderT S.runServer env
            hClose fileHandle)
    
    
