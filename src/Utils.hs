module Utils where

import Web.Fn (okHtml)
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Network.Wai (Response)
import Lucid (Html, renderText)

showT :: Show a => a -> Text
showT = pack . show

readT :: Read a => Text -> a
readT = read . unpack

lucidHtml :: Html () -> IO (Maybe Response)
lucidHtml h = okHtml $ toStrict $ renderText h
