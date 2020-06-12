module Env
    ( Env   (..)
    , HasLog (..)
    ) where

import qualified Data.Text as T
import Data.IORef
import Prelude hiding (error)

data Env = Env 
    { envLog                    :: !(T.Text -> IO ())
    , debug                     :: !(T.Text -> IO ())
    , info                      :: !(T.Text -> IO ())
    , warning                   :: !(T.Text -> IO ())
    , error                     :: !(T.Text -> IO ())
    , updateID                  :: !(IORef Integer)
    , repeatCount               :: !(IORef Integer)
    , helpMessage               :: !T.Text
    , repeateQuestion           :: !T.Text
    , botToken                  :: !T.Text  
    , pollTimeoutMicroseconds   :: !Integer
    }

-- can be applied to other types of environments
class HasLog a where 
    getLog :: a -> (T.Text -> IO ())

instance HasLog Env where
    getLog = envLog


