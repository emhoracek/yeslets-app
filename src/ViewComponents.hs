{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ViewComponents where

import           Web.Fn (okHtml)
import           Network.Wai (Response)
import           Data.Text (Text)
import           Data.Text.Lazy (toStrict)
import           Lucid

siteHeader :: Text -> Html ()
siteHeader pageTitle = do
  html_ $ do
    head_ $ do
      title_ (toHtml pageTitle)
      link_ [ rel_ "stylesheet"
            , href_ "style.css"]
    body_ $ do
      header_ $ do
        h1_ "Yeslets!"
        p_ $ do
          a_ [href_ "/login"] "Log in"
