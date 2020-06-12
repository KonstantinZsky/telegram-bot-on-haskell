module Config
       ( Config (..)
       , loadConfig
       , getDefaultConfig
       , writeDefaultConfig
       , defaultConfigContents
       ) where

import qualified Data.Text.IO as T
import qualified Data.Text    as T
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (when)
import Data.Maybe (fromMaybe, isNothing)
import Data.Configurator (lookup, load)
import Data.Configurator.Types (Value, Configured, Worth(Required), convert)
import Prelude hiding (lookup, log)

import Logger (Verbosity (..), log)

import Concole (askUser)

instance Configured Verbosity where
    convert v = case (convert v :: Maybe T.Text) of
        (Just "Debug")      -> Just Debug
        (Just "Info")       -> Just Info
        (Just "Warning")    -> Just Warning
        (Just "Error")      -> Just Error
        _                   -> Nothing

newtype GreaterThanOne = GreaterThanOne Integer

instance Configured GreaterThanOne where
    convert v = case (convert v :: Maybe Integer) of
        (Just a)            -> if a < 1 then Nothing else Just $ GreaterThanOne a
        Nothing             -> Nothing

instance Show GreaterThanOne where
    show (GreaterThanOne a) = show a

unwrap :: Maybe GreaterThanOne -> Maybe Integer
unwrap (Just (GreaterThanOne a)) = Just a
unwrap Nothing = Nothing

-- | Data type for the configurable elements of the application.
data Config = Config
    { cLogVerbosity             :: !Verbosity
    , cHelpMessage              :: !T.Text
    , cRepeatQuestion           :: !T.Text
    , cRepeatCount              :: !Integer
    , cPollTimeoutMicroseconds  :: !Integer
    , cBotToken                 :: !T.Text
    } deriving Show

getConfig :: Maybe Verbosity -> Maybe T.Text -> Maybe T.Text -> Maybe Integer -> Maybe Integer -> Maybe T.Text -> Config
getConfig v h rq rc p b = Config
    { cLogVerbosity = fromMaybe Debug v
    , cHelpMessage = fromMaybe "Echo bot, returns messages back to the user." h
    , cRepeatQuestion = fromMaybe "Choose the number of bot response duplication." rq
    , cRepeatCount = fromMaybe 1 rc
    , cPollTimeoutMicroseconds = fromMaybe 5000000 p
    , cBotToken = fromMaybe "" b}

getDefaultConfig :: Config
getDefaultConfig = getConfig Nothing Nothing Nothing Nothing Nothing Nothing

configContents :: Config -> T.Text
configContents Config   { cLogVerbosity = v
                        , cHelpMessage = h
                        , cRepeatQuestion = rq
                        , cRepeatCount = rc
                        , cPollTimeoutMicroseconds = p
                        , cBotToken = b} = 
    "# Verbosity level from wich messagies will show up, can be: \"Debug\", \"Info\", \"Warning\", \"Error\". Must be in quotes. \n" <> 
    "cLogVerbosity = \""            <> (T.pack $ show v) <> "\" \n" <>
    "# Message that will be shown after command /help. Must be in quotes. \n" <>
    "cHelpMessage = \""             <> h <> "\" \n" <>
    "# Question that will be shown after command /repeat. Must be in quotes. \n" <>
    "cRepeatQuestion = \""          <> rq <> "\" \n" <>
    "# Number of bot response duplication. \n" <>
    "# Must be greater then 1. \n"  <>
    "cRepeatCount = "               <> (T.pack $ show rc) <> " \n" <>
    "# Long polling timeout microseconds, 1s = 1000000. \n" <>
    "# Must be greater then 1. \n"  <>
    "cPollTimeoutMicroseconds = "   <> (T.pack $ show p) <> " \n" <>
    "# Bot token for qonnection to the API. Must be in quotes. \n" <>
    "cBotToken = \""                <> b <> "\" \n"

defaultConfigContents :: T.Text
defaultConfigContents = configContents getDefaultConfig

writeConfig :: FilePath -> T.Text -> IO ()
writeConfig path contens = T.writeFile path contens

writeDefaultConfig :: FilePath -> IO ()
writeDefaultConfig path = writeConfig path (configContents getDefaultConfig)

-- can throw exceptions when writing files
loadConfig :: FilePath -> IO Config
loadConfig path = do
    rawCfg <- load [Required path]
    v <- lookup rawCfg "cLogVerbosity" :: IO (Maybe Verbosity)
    when (isNothing v) $ log Warning "Reading config: field cLogVerbosity missing or wrong value"
    h <- lookup rawCfg "cHelpMessage" :: IO (Maybe T.Text)
    when (isNothing h) $ log Warning "Reading config: field cHelpMessage missing or wrong value"
    rq <- lookup rawCfg "cRepeatQuestion" :: IO (Maybe T.Text)
    when (isNothing rq) $ log Warning "Reading config: field cRepeatQuestion missing or wrong value"
    rc <- lookup rawCfg "cRepeatCount" :: IO (Maybe GreaterThanOne)
    when (isNothing rc) $ log Warning "Reading config: field cRepeatCount missing or wrong value"
    p <- lookup rawCfg "cPollTimeoutMicroseconds" :: IO (Maybe GreaterThanOne)
    when (isNothing p) $ log Warning "Reading config: field cPollTimeoutMicroseconds missing or wrong value"
    b <- lookup rawCfg "cBotToken" :: IO (Maybe T.Text)
    when (isNothing b) $ log Warning "Reading config: field cBotToken missing or wrong value"    
    let cfg = getConfig v h rq (unwrap rc) (unwrap p) b
    when (isNothing (v>>h>>rq>>rc>>p>>b) ) 
        (askUser "Do you want to rewrite missing fields of the config with default values? (Y/y) (Any other letter for reject)\n" 
            (writeConfig path (configContents cfg) >> log Info "Config renewed")
            (log Warning "Misssing fields in config"))
    return cfg