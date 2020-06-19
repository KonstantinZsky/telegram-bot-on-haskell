module Main where

import qualified Data.Text      as T
import qualified System.Exit    as E
import Control.Monad (when)
import Control.Exception (bracket)
import System.IO (openFile, hClose, IOMode(WriteMode))
import Control.Monad.Trans.Reader

import qualified Files          as F
import qualified Config         as C
import qualified Env.Init       as E
import qualified Server         as S
import Concole (askUser)
import Control.Exception.Extends (catchLogRethrow)

main :: IO ()
main = do
    F.checkForConfig "test.cfg"
    cfg <- catchLogRethrow "Error while reading config " (C.loadConfig "test.cfg")
    putStrLn $ show cfg
    when (C.cBotToken cfg == "") $ do
        askUser "Bot token is not specifed. You must write it in config manually. (Put any char to quit program)" (return ()) (return ())
        E.exitSuccess
    F.checkForFile "Log file not found. Creating " "log.txt" ""
    bracket
        (openFile "log.txt" WriteMode)
        (hClose)
        (\fileHandle -> do
            env <- E.initEnv fileHandle cfg
            runReaderT S.runServer env
            hClose fileHandle)
    
    
