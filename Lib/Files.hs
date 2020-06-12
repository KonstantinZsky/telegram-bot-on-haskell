module Files where

import qualified Data.Text.IO as T
import qualified Data.Text    as T

import qualified Control.Exception as E

import System.Directory (doesFileExist)
import Prelude hiding (log)

import Logger (log, Verbosity(..))

import Config (writeDefaultConfig)

checkForFile :: T.Text -> FilePath -> IO ()
checkForFile msg path = do
    chk <- doesFileExist path
    case chk of
        True  -> return ()
        False -> do
            log Info $ msg <> T.pack path <> "."
            logFileErrors $ writeDefaultConfig path    

checkForConfig :: FilePath -> IO ()
checkForConfig = checkForFile "Config not found. Creating "

logFileErrors :: IO a -> IO a
logFileErrors action = do
    E.catch action $ rethrowError "Unexpected error while working with file: "

rethrowError ::  T.Text -> E.SomeException -> IO a
rethrowError msg err = log Error msg >> putStrLn (show err) >> E.throwIO err
