module Main where

import qualified Data.Text      as T
import qualified System.Exit    as E
import Control.Monad (when)

import Logger (Verbosity (..), MonadLog, makeLoggingFunction)

import qualified Config as C

import qualified Files as F

import Concole (askUser)

import Prelude hiding (error)

verbosityToOutput :: Verbosity
verbosityToOutput = Debug

debug, info, warning, error :: (MonadLog m) => T.Text -> m ()
debug   = makeLoggingFunction verbosityToOutput Debug
info    = makeLoggingFunction verbosityToOutput Info
warning = makeLoggingFunction verbosityToOutput Warning
error   = makeLoggingFunction verbosityToOutput Error

main :: IO ()
main = do
    F.checkForConfig "test.cfg"
    cfg <- F.logFileErrors $ C.loadConfig "test.cfg"
    putStrLn $ show cfg -- (C.cBotToken cfg == "") 
    -- askUser "Bot token is not specifed. You must write it in config manually. (Put any char to quit program)" (return ()) (return ())
    when (C.cBotToken cfg == "") $ do
        askUser "Bot token is not specifed. You must write it in config manually. (Put any char to quit program)" (return ()) (return ())
        E.exitSuccess
    putStrLn "next step"
    

