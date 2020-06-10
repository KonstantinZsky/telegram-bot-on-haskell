module ConcoleInteraction where

import Control.Monad.IO.Class (MonadIO, liftIO) 

import Data.Char (toLower)

import qualified Data.Text.IO as T
import qualified Data.Text    as T

askUser :: MonadIO m => T.Text -> m a -> m a -> m a
askUser msg first_action second_action = do
    liftIO $ T.putStrLn msg
    answer <- liftIO $ getChar
    case toLower answer of
        'y' -> first_action
        _   -> second_action

