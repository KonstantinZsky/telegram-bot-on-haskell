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
    -- r <- get $ "https://api.telegram.org/bot1172228691:AAGjAhdw62-zoPGMD-ot3h-c9vo0o19FzGc/getUpdates"
    let code = r ^. responseStatus . statusCode
    log Debug $ T.pack $ show $ r ^? responseBody
    --putStrLn $ show $ r ^? header

getTeleramUpdate :: T.Text -> IO B.ByteString
getTeleramUpdate con = do
    r <- F.logErrors "Can not connect to the telegram bot. Possibly wrong token. Exiting program." $ 
        get $ T.unpack con
    return $ fromMaybe "" $ r ^? responseBody

--post "http://httpbin.org/post"

handleMessages :: BotData -> ReaderT Env IO ()
handleMessages BotData {result = []} = do
    uID <- updateID <$> ask
    liftIO $ writeIORef uID (-1) 
handleMessages btd = do
    let firstMsg = head $ result btd
    let upid = update_id firstMsg
    let outTxt = text $ message firstMsg
    let chatID = chat_id $ chat $ message firstMsg  
    liftIO $ post ("https://api.telegram.org/bot1172228691:AAGjAhdw62-zoPGMD-ot3h-c9vo0o19FzGc/sendMessage?chat_id=" <> show chatID <> "&text=" <> T.unpack outTxt ) ("" :: B.ByteString)
    uID <- updateID <$> ask
    liftIO $ writeIORef uID $ toEnum (upid+1) 
