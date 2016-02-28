{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ViewComponents where

import           Data.Text (Text)
import           Lucid
import           Context

siteHeader :: Text -> Bool -> Html ()
siteHeader pageTitle loggedIn= do
  html_ $ do
    head_ $ do
      title_ (toHtml pageTitle)
      link_ [ rel_ "stylesheet"
            , href_ "style.css"]
    body_ $ do
      header_ $ do
        h1_ "Yeslets!"
        p_ $ do
          if loggedIn
          then a_ [href_ "/logout"] "Log out"
          else a_ [href_ "/login"] "Log in"
