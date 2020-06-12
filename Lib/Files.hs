module Files where

import qualified Data.Text.IO as T
import qualified Data.Text    as T

import qualified Control.Exception as E

import System.Directory (doesFileExist)
import Prelude hiding (log)

import Logger (log, Verbosity(..))

import Config (defaultConfigContents)

checkForFile :: T.Text -> FilePath -> T.Text -> IO ()
checkForFile msg path content = do
    chk <- doesFileExist path
    case chk of
        True  -> return ()
        False -> do
            log Info $ msg <> T.pack path <> "."
            logFileErrors $ T.writeFile "log.txt" content   

checkForConfig :: FilePath -> IO ()
checkForConfig path = checkForFile "Config not found. Creating " path defaultConfigContents

rethrowError ::  T.Text -> E.SomeException -> IO a
rethrowError msg err = log Error msg >> E.throwIO err

logErrors :: T.Text -> IO a -> IO a
logErrors msg action = E.catch action $ rethrowError msg

logFileErrors :: IO a -> IO a
logFileErrors = logErrors "Unexpected error while working with file: "
