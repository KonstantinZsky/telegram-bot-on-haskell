module Config.Logic
       ( Config (..)
       , loadConfig
       , defaultConfigContents
       ) where

import qualified Data.Text.IO as T
import qualified Data.Text    as T
import Control.Monad (when)
import Data.Maybe (fromMaybe, isNothing)
import Data.Configurator (lookup, load)
import Data.Configurator.Types (Value, Configured, Worth(Required), convert)
import Prelude hiding (lookup)

import Config.Mode (Mode(..))
import Config.Parsing (GreaterThanOne(..), unwrap)
import Logging.Logger (warning, info)
import Logging.Verbosity (Verbosity(..))
import Concole (askUser)

-- | Data type for the configurable elements of the application.
data Config = Config
    { cMode                     :: !Mode
    , cLogVerbosity             :: !Verbosity
    , cHelpMessage              :: !T.Text
    , cRepeatQuestion           :: !T.Text
    , cRepeatCount              :: !Integer
    , cPollTimeoutMicroseconds  :: !Integer
    , cBotToken                 :: !T.Text
    } deriving Show

-- can throw exceptions when writing files
loadConfig :: FilePath -> IO Config
loadConfig path = do
    rawCfg <- load [Required path]
    m <- lookup rawCfg "cMode" :: IO (Maybe Mode)
    when (isNothing m) $ warning "Reading config: field cMode missing or wrong value"    
    v <- lookup rawCfg "cLogVerbosity" :: IO (Maybe Verbosity)
    when (isNothing v) $ warning "Reading config: field cLogVerbosity missing or wrong value"
    h <- lookup rawCfg "cHelpMessage" :: IO (Maybe T.Text)
    when (isNothing h) $ warning "Reading config: field cHelpMessage missing or wrong value"
    rq <- lookup rawCfg "cRepeatQuestion" :: IO (Maybe T.Text)
    when (isNothing rq) $ warning "Reading config: field cRepeatQuestion missing or wrong value"
    rc <- lookup rawCfg "cRepeatCount" :: IO (Maybe GreaterThanOne)
    when (isNothing rc) $ warning "Reading config: field cRepeatCount missing or wrong value"
    p <- lookup rawCfg "cPollTimeoutMicroseconds" :: IO (Maybe GreaterThanOne)
    when (isNothing p) $ warning "Reading config: field cPollTimeoutMicroseconds missing or wrong value"
    b <- lookup rawCfg "cBotToken" :: IO (Maybe T.Text)
    when (isNothing b) $ warning "Reading config: field cBotToken missing or wrong value"    
    let cfg = getConfig m v h rq (unwrap rc) (unwrap p) b
    when (isNothing (m>>v>>h>>rq>>rc>>p>>b) ) 
        (askUser "Do you want to rewrite missing fields of the config with default values? (Y/y) (Any other letter for reject)\n" 
            (T.writeFile path (configContents cfg) >> info "Config renewed")
            (warning "Misssing fields in config"))
    return cfg

configContents :: Config -> T.Text
configContents Config   { cMode = m
                        , cLogVerbosity = v
                        , cHelpMessage = h
                        , cRepeatQuestion = rq
                        , cRepeatCount = rc
                        , cPollTimeoutMicroseconds  = p
                        , cBotToken = b} = 
    "# Mode - social network to connect to: \"TG\" for telegram, \"VK\" for vkontakte. Must be in quotes.\n" <>
    "cMode = \""                    <> (T.pack $ show m) <> "\" \n\n" <>
    "# Verbosity level from wich messagies will show up, can be: " <>
        "\"Debug\", \"Info\", \"Warning\", \"Error\". Must be in quotes. \n" <> 
    "cLogVerbosity = \""            <> (T.pack $ show v) <> "\" \n\n" <>
    "# Message that will be shown after command /help. Must be in quotes. \n" <>
    "cHelpMessage = \""             <> h <> "\" \n\n" <>
    "# Question that will be shown after command /repeat. Must be in quotes. \n" <>
    "cRepeatQuestion = \""          <> rq <> "\" \n\n" <>
    "# Number of duplication for bot response. \n" <>
    "# Must be greater then 1. \n"  <>
    "cRepeatCount = "               <> (T.pack $ show rc) <> " \n\n" <>
    "# Long polling timeout microseconds, 1s = 1000000. \n" <>
    "# Must be greater then 1. \n"  <>
    "cPollTimeoutMicroseconds = "   <> (T.pack $ show p) <> " \n\n" <>
    "# Bot token for qonnection to the API. Must be in quotes. \n" <>
    "cBotToken = \""                <> b <> "\" \n"

getConfig :: Maybe Mode -> Maybe Verbosity -> Maybe T.Text -> Maybe T.Text 
                -> Maybe Integer -> Maybe Integer -> Maybe T.Text -> Config
getConfig m v h rq rc p b = Config
    { cMode                     = fromMaybe TG m
    , cLogVerbosity             = fromMaybe Debug v
    , cHelpMessage              = fromMaybe "Echo bot, returns messages back to the user." h
    , cRepeatQuestion           = fromMaybe "Choose the number of bot response duplication." rq
    , cRepeatCount              = fromMaybe 1 rc
    , cPollTimeoutMicroseconds  = fromMaybe 5000000 p
    , cBotToken                 = fromMaybe "" b}

getDefaultConfig :: Config
getDefaultConfig = getConfig Nothing Nothing Nothing Nothing Nothing Nothing Nothing

defaultConfigContents :: T.Text
defaultConfigContents = configContents getDefaultConfig