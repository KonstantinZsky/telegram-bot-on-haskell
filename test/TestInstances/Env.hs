
module TestInstances.Env where

import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import Data.HashMap

import Logger.Verbosity

data TEnv mode a = Env
    { verbosity                 :: !Verbosity
    , updateID                  :: !Integer
    , repeatCount               :: !Integer
    , cpuTimestamp              :: !Integer
    , supportDataString         :: !T.Text
    , helpMessage               :: !T.Text
    , repeateQuestion           :: !T.Text
    , botToken                  :: !T.Text 
    , groupID                   :: !Integer 
    , pollTimeoutMicroseconds   :: !Integer
    , maximumMessageFrequency   :: !Integer
    , testHashMap               :: !a 
    , testOutput                :: ![T.Text]
    , testInput                 :: ![ByteString]
    }