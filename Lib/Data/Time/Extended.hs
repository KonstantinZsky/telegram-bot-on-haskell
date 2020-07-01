module Data.Time.Extended where

import qualified Data.Text              as T
import qualified Data.Time.LocalTime    as Time
import Control.Monad.IO.Class (MonadIO, liftIO) 

getCurrentTime :: MonadIO m => m T.Text
getCurrentTime = do
    time <- liftIO $ Time.getZonedTime
    let localTime = Time.zonedTimeToLocalTime time
    return $ T.pack $ show localTime

piko :: Integer
piko = 10^12

micro :: Integer
micro = 10^6

secondsToCPU :: Integer -> Integer
secondsToCPU a = a * piko

cpuToMicro :: Integer -> Int
cpuToMicro a = fromEnum $ div a micro





