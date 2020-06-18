module Files where

import qualified Data.Text.IO as T
import qualified Data.Text    as T
import qualified Control.Exception.Extends as E
import System.Directory (doesFileExist)
import Prelude hiding (log)

import qualified Logger as L
import Config (defaultConfigContents)


checkForFile :: T.Text -> FilePath -> T.Text -> IO ()
checkForFile msg path content = do
    chk <- doesFileExist path
    case chk of
        True  -> return ()
        False -> do
            L.info $ msg <> T.pack path <> "."
            E.catchLogRethrow "Unexpected error while working with file: " $ T.writeFile "log.txt" content  

checkForConfig :: FilePath -> IO ()
checkForConfig path = checkForFile "Config not found. Creating " path defaultConfigContents

