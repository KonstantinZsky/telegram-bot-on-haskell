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
import qualified Env            as E
import qualified Server         as S
import qualified Web.Types      as W
import Config.Mode (Mode(..))
import Concole (askUser)
import Control.Exception.Extends (catchLogRethrow)

main :: IO ()
main = do
    F.checkForConfig "test.cfg"
    cfg <- catchLogRethrow "Error while reading config " (C.loadConfig "test.cfg")
    putStrLn $ show cfg
    when (C.cBotToken cfg == "") $ do
        askUser ("Bot token is not specifed. " <> 
            "You must write it in config manually. (Put any char to quit the program)") (return ()) (return ())
        E.exitSuccess
    when (C.cMode cfg == VK && C.cGroupID cfg == 0) $ do
        askUser ("Mode is VK but group ID is not specifed. " <> 
            "You must write it in config manually. (Put any char to quit the program)") (return ()) (return ())
        E.exitSuccess
    F.checkForFile "Log file not found. Creating " "log.txt" ""
    bracket
        (openFile "log.txt" WriteMode)
        (hClose)
        (\fileHandle -> do
            case C.cMode cfg of 
                TG -> do
                    env <- E.initEnv fileHandle cfg
                    runReaderT S.runServer (env :: E.Env W.Telegram)
                    hClose fileHandle
                VK -> undefined
                {- do
                    env <- E.initEnv fileHandle cfg
                    runReaderT S.runServer (env :: E.Env W.Vkontakte)
                    hClose fileHandle
                -}
                
                )
    
    
