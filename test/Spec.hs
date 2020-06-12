import qualified Data.Text as T
import qualified Control.Exception as E

import Logger (Verbosity (..), MonadLog, makeLoggingFunction)

import Config

import qualified Files as F

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
    cfg <- F.logFileErrors $ loadConfig "test.cfg"
    putStrLn $ show cfg
    debug "тест Debug"
    error "тест-протест error"
