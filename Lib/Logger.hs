 {-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Logger where

import Prelude hiding (log)

import qualified Data.Text      as T
import qualified Data.Text.IO   as T

import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Time.LocalTime as Time

import Env (Env, HasLog (getLog))

data Verbosity
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show)

class Monad m => MonadLog m where
    pushLogMessage :: T.Text -> m ()

getCurrentTime :: MonadIO m => m T.Text
getCurrentTime = do
    time <- liftIO $ Time.getZonedTime
    let localTime = Time.zonedTimeToLocalTime time
    return $ T.pack $ show localTime

-- can remove MonadIO and getCurrentTime and put it into envLog realisation
instance (HasLog env, MonadIO m) => MonadLog (ReaderT env m) where
    pushLogMessage msg = do
        env <- ask
        time <- getCurrentTime
        liftIO $ getLog env $ time <> " " <> msg

-- logging before we parse config and get ReaderT: output into concole
{-
instance (Monad m, MonadIO m) => MonadLog m where
     pushLogMessage msg = do
         time <- getCurrentTime
         liftIO $ T.putStrLn $ time <> " " <> msg -}   
instance MonadLog IO where
     pushLogMessage msg = do
         time <- getCurrentTime
         T.putStrLn $ time <> " " <> msg


-- pure logging function
log :: (MonadLog m) => Verbosity -> T.Text -> m ()
log v msg = pushLogMessage $ (T.pack $ show v) <> ": " <> msg


-- for the disabling of logging of chosen verbosity levels
makeLoggingFunction :: (MonadLog m) => Verbosity -> Verbosity -> T.Text -> m ()
makeLoggingFunction v_chk v msg     | v >= v_chk = log v msg 
                                    | otherwise = return ()