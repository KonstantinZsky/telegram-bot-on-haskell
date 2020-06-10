import qualified Data.Text      as T

import Logger (Verbosity (..), MonadLog, makeLoggingFunction)

import Prelude hiding (error)

verbosityToOutput :: Verbosity
verbosityToOutput = Debug

debug, info, warning, error :: (MonadLog m) => T.Text -> m ()
debug   = makeLoggingFunction verbosityToOutput Debug
info    = makeLoggingFunction verbosityToOutput Info
warning = makeLoggingFunction verbosityToOutput Warning
error   = makeLoggingFunction verbosityToOutput Error

main :: IO ()
main = do
    debug "тест Debug"
    error "тест-протест error"
