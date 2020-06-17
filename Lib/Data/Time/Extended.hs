module Data.Time.Extended (getCurrentTime) where

import qualified Data.Text              as T
import qualified Data.Time.LocalTime    as Time
import Control.Monad.IO.Class (MonadIO, liftIO) 

getCurrentTime :: MonadIO m => m T.Text
getCurrentTime = do
    time <- liftIO $ Time.getZonedTime
    let localTime = Time.zonedTimeToLocalTime time
    return $ T.pack $ show localTime


