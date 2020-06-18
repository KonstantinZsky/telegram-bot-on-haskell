module Logger (MonadLog (debug, info, warning, error))where

import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad (when)
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import Prelude hiding (log, error)

import Data.Time.Extended (getCurrentTime)
import Env (Env, HasLog (getLog, getVerbosity))
import Logger.Verbosity (Verbosity(..))

class Monad m => MonadLog m where
    pushLogMessage :: T.Text -> m ()
    log     :: Verbosity -> T.Text -> m () -- pure logging function
    log v msg = pushLogMessage $ (T.pack $ show v) <> ": " <> msg
    debug   :: T.Text -> m ()
    info    :: T.Text -> m ()
    warning :: T.Text -> m ()
    error   :: T.Text -> m ()    

-- pure logging in ReaderT monad
--  curren time is added to the message in getLog realization 
instance HasLog env m => MonadLog (ReaderT env m) where
    pushLogMessage msg = ReaderT $ \env -> getLog env msg
    debug   msg = instanceHelper Debug      msg
    info    msg = instanceHelper Info       msg
    warning msg = instanceHelper Warning    msg
    error   msg = instanceHelper Error      msg
         
instanceHelper :: HasLog env m => Verbosity -> T.Text -> ReaderT env m ()
instanceHelper v' msg = do
    v <- ReaderT $ getVerbosity
    if (v > v') then return () else log v' msg

-- logging before we parse config and get ReaderT: output into concole  
instance MonadLog IO where
    pushLogMessage msg = do
        time <- getCurrentTime
        T.putStrLn $ time <> " " <> msg
    debug   msg = log Debug     msg
    info    msg = log Info      msg
    warning msg = log Warning   msg
    error   msg = log Error     msg
