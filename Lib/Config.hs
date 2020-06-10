module Config
       ( Config (..)
       , loadConfig
       , getDefaultConfig
       ) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Text.IO as T
import qualified Data.Text    as T

import Control.Monad (when)
import Data.Maybe (fromMaybe, isNothing)

import Prelude hiding (lookup, log)

import Data.Configurator
import Data.Configurator.Types (Value, Configured, convert)

import Logger (Verbosity (..), log)

import ConcoleInteraction (askUser)

-- | Data type for the configurable elements of the application.
data Config = Config
    { cLogVerbosity             :: !Verbosity
    , cHelpMessage              :: !T.Text
    , cRepeatQuestion           :: !T.Text
    , cRepeatCount              :: !Integer
    , cPollTimeoutMicroseconds  :: !Integer
    } deriving Show

instance Configured Verbosity where
    convert v = case (convert v :: Maybe T.Text) of
        (Just "Debug")      -> Just Debug
        (Just "Info")       -> Just Info
        (Just "Warning")    -> Just Warning
        (Just "Error")      -> Just Error
        _                   -> Nothing

getConfig :: Maybe Verbosity -> Maybe T.Text -> Maybe T.Text -> Maybe Integer -> Maybe Integer -> Config
getConfig v h rq rc p = Config
    { cLogVerbosity = fromMaybe Debug v
    , cHelpMessage = fromMaybe "Echo bot, returns messages back to the user." h
    , cRepeatQuestion = fromMaybe "Choose the number of bot response duplication." rq
    , cRepeatCount = fromMaybe 1 rc
    , cPollTimeoutMicroseconds = fromMaybe 5000000 p}

getDefaultConfig :: Config
getDefaultConfig = getConfig Nothing Nothing Nothing Nothing Nothing

configContents :: Config -> T.Text
configContents Config   { cLogVerbosity = v
                        , cHelpMessage = h
                        , cRepeatQuestion = rq
                        , cRepeatCount = rc
                        , cPollTimeoutMicroseconds = p} = 
    "# Verbosity level from wich messagies will show up, can be: \"Debug\", \"Info\", \"Warning\", \"Error\" \n" <> 
    "cLogVerbosity = \"" <> (T.pack $ show v) <> "\" \n" <>
    "# Message that will be shown after command /help" <>
    "cHelpMessage = \"" <> h <> "\" \n" <>
    "# Question that will be shown after command /repeat" <>
    "cRepeatQuestion = \"" <> rq <> "\" \n" <>
    "# Number of bot response duplication" <>
    "cRepeatCount = \"" <> (T.pack $ show rc) <> "\" \n" <>
    "# Long polling timeout microseconds, 1 s = 1000000" <>
    "cRepeatCount = \"" <> (T.pack $ show p) <> "\" \n"

defaultConfigContents :: T.Text
defaultConfigContents = configContents getDefaultConfig

writeConfig :: FilePath -> T.Text -> IO ()
writeConfig path contens = do
    T.writeFile path contens

loadConfig :: IO Config
loadConfig = do
    rawCfg <- load [Required "config.cfg"]
    v <- lookup rawCfg "cLogVerbosity" :: IO (Maybe Verbosity)
    when (isNothing v) $ log Warning "Reading config: field cLogVerbosity missing"
    h <- lookup rawCfg "cHelpMessage" :: IO (Maybe T.Text)
    when (isNothing v) $ log Warning "Reading config: field cHelpMessage missing"
    rq <- lookup rawCfg "cRepeatQuestion" :: IO (Maybe T.Text)
    when (isNothing v) $ log Warning "Reading config: field cRepeatQuestion missing"
    rc <- lookup rawCfg "cRepeatCount" :: IO (Maybe Integer)
    when (isNothing rc) $ log Warning "Reading config: field cRepeatCount missing"
    p <- lookup rawCfg "cPollTimeoutMicroseconds" :: IO (Maybe Integer)
    when (isNothing rc) $ log Warning "Reading config: field cPollTimeoutMicroseconds missing"
    let cfg = getConfig v h rq rc p
    when (isNothing (v>>h>>rq>>rc>>p) ) 
        (askUser "Do you want to rewrite missing fields of config with default values? (Y/y) (N/n)\n" 
            (writeConfig "config.cfg" $ configContents cfg)
            (log Warning "Misssing fields in config"))
    return cfg