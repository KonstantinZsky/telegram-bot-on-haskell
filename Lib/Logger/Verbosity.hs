module Logger.Verbosity where

data Verbosity
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show)