import qualified Data.Text as T
import qualified Control.Exception as E

import Logging.Verbosity
import Logging.Logger (MonadLog)

import Config.Logic

import qualified Files as F

import Prelude hiding (error)

main :: IO ()
main = do
    F.checkForConfig "test.cfg"
    cfg <- F.logFileErrors $ loadConfig "test.cfg"
    putStrLn $ show cfg

