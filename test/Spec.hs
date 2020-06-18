import qualified Data.Text as T
import qualified Control.Exception as E

import Logger.Verbosity
import Logger (MonadLog)

import Config

import qualified Files as F

import Prelude hiding (error)

main :: IO ()
main = do
    F.checkForConfig "test.cfg"
    cfg <- F.logFileErrors $ loadConfig "test.cfg"
    putStrLn $ show cfg

