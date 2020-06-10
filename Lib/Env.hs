module Env
    ( Env (..)
    , HasLog (..)
    ) where

import qualified Data.Text as T

data Env = Env 
    {   envLog :: !(T.Text -> IO ())
    }

class HasLog a where
    getLog :: a -> (T.Text -> IO ())

instance HasLog Env where
    getLog = envLog


