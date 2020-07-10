module Config.Parsing where

import qualified Data.Text as T
import Data.Configurator.Types
import Data.Configurator

import Logger.Verbosity (Verbosity(..))
import Config.Mode (Mode(..))

instance Configured Verbosity where
    convert v = case (convert v :: Maybe T.Text) of
        (Just "Debug")      -> Just Debug
        (Just "Info")       -> Just Info
        (Just "Warning")    -> Just Warning
        (Just "Error")      -> Just Error
        _                   -> Nothing

newtype GreaterThanZero = GreaterThanZero Integer

instance Configured GreaterThanZero where
    convert v = case (convert v :: Maybe Integer) of
        (Just a)            -> if a < 1 then Nothing else Just $ GreaterThanZero a
        Nothing             -> Nothing

instance Show GreaterThanZero where
    show (GreaterThanZero a) = show a

unwrap :: Maybe GreaterThanZero -> Maybe Integer
unwrap (Just (GreaterThanZero a)) = Just a
unwrap Nothing = Nothing

instance Configured Mode where
    convert v = case (convert v :: Maybe T.Text) of
        (Just "TG")         -> Just TG
        (Just "VK")         -> Just VK
        _                   -> Nothing