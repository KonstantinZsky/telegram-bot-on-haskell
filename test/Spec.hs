import qualified Data.Text as T
import qualified Control.Exception as E

import Logger (Verbosity (..), MonadLog, makeLoggingFunction)

import Config

import Prelude hiding (error)

verbosityToOutput :: Verbosity
verbosityToOutput = Debug

debug, info, warning, error :: (MonadLog m) => T.Text -> m ()
debug   = makeLoggingFunction verbosityToOutput Debug
info    = makeLoggingFunction verbosityToOutput Info
warning = makeLoggingFunction verbosityToOutput Warning
error   = makeLoggingFunction verbosityToOutput Error

handleError :: E.SomeException -> IO Config
handleError err = (putStrLn $ "?!?! " <> show err) >> return getDefaultConfig-- >> E.throwIO err

main :: IO ()
main = do
    cfg <- E.catch (loadConfig) handleError
    putStrLn $ show cfg
    debug "тест Debug"
    error "тест-протест error"
