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

newtype GreaterThanOne = GreaterThanOne Integer

instance Configured GreaterThanOne where
    convert v = case (convert v :: Maybe Integer) of
        (Just a)            -> if a < 1 then Nothing else Just $ GreaterThanOne a
        Nothing             -> Nothing

instance Show GreaterThanOne where
    show (GreaterThanOne a) = show a

unwrap :: Maybe GreaterThanOne -> Maybe Integer
unwrap (Just (GreaterThanOne a)) = Just a
unwrap Nothing = Nothing

instance Configured Mode where
    convert v = case (convert v :: Maybe T.Text) of
        (Just "TG")         -> Just TG
        (Just "VK")         -> Just VK
        _                   -> Nothing