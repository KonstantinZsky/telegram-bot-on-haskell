module Web.Telegram.HTTP where

import Data.IORef
import Control.Lens
import Network.Wreq
import Network.Wreq.Types
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (MonadIO, liftIO) 
import qualified Network.Wreq.Session as S
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified System.Exit    as E

import Prelude hiding (log)

import Web.Telegram.Parsing
import Logger (Verbosity (..), log)
import qualified Files as F
import Env

checkTelegramConnection :: T.Text -> IO ()
checkTelegramConnection token = do
    r <- F.logErrors "Can not connect to the telegram bot. Possibly wrong token. Exiting program." $ 
        get $ "https://api.telegram.org/bot" <> T.unpack token <> "/getUpdates"
    let code = r ^. responseStatus . statusCode
    log Debug $ T.pack $ show $ r ^? responseBody
    --putStrLn $ show $ r ^? header

getTeleramUpdate :: T.Text -> IO B.ByteString
getTeleramUpdate con = do
    r <- F.logErrors "Can not connect to the telegram bot. Possibly wrong token. Exiting program." $ 
        get $ T.unpack con
    return $ fromMaybe "" $ r ^? responseBody

--post "http://httpbin.org/post"

handleMessages :: BotData -> T.Text -> ReaderT Env IO ()
handleMessages BotData {result = []} _ = do
    uID <- updateID <$> ask
    liftIO $ writeIORef uID (-1) 
handleMessages btd token = do
    let firstMsg = head $ result btd
    let upid = update_id firstMsg
    let outTxt = T.replace "#" "%23" $text $ message firstMsg
    let chatID = chat_id $ chat $ message firstMsg 
    log Debug $ outTxt
    --liftIO $ T.putStrLn outTxt
    --liftIO $ putStrLn $ T.unpack outTxt
    liftIO $ post ("https://api.telegram.org/bot" <> T.unpack token <> "/sendMessage?chat_id=" <> show chatID <> "&text=" <> T.unpack outTxt ) ("" :: B.ByteString)
    uID <- updateID <$> ask
    liftIO $ writeIORef uID $ toEnum (upid+1) 
