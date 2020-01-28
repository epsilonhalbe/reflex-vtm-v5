module LocalStorage where

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.Storage as GHCJS
import           GHCJS.DOM.Types (fromJSString, JSString, toJSString, MonadJSM, liftJSM, Storage)
import qualified Data.Aeson as Aeson
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL

getLocalStorage :: MonadJSM m => m Storage
getLocalStorage
  = liftJSM $ Window.getLocalStorage
          =<< DOM.currentWindowUnchecked

getItem
  :: ( FromJSON result
     , MonadJSM m
     )
  => GHCJS.Storage -> JSString -> m (Maybe result)
getItem storage key
  = (fromJsonString =<<) <$> GHCJS.getItem storage key

setItem
  :: ( MonadJSM m
     , ToJSON value
     )
  => GHCJS.Storage -> JSString -> value -> m ()
setItem storage key value
  = GHCJS.setItem storage key $ toJsonString value

removeItem
  :: ( MonadJSM m
     )
  => GHCJS.Storage -> JSString -> m ()
removeItem storage key
  = GHCJS.removeItem storage key

toJsonString :: ToJSON a => a -> JSString
toJsonString
  = toJSString
  . T.decodeUtf8
  . BL.toStrict
  . Aeson.encode

fromJsonString :: FromJSON a => JSString -> Maybe a
fromJsonString
  = Aeson.decodeStrict
  . T.encodeUtf8
  . fromJSString
