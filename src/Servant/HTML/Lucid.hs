{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Servant.HTML.Lucid (HTML) where

import Servant.API.ContentTypes (Accept(..), MimeRender(..))
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Lucid (Html, renderText)
import Network.HTTP.Media ((//), (/:))

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML (Html a) where
  mimeRender _ = encodeUtf8 . renderText
