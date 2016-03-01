{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module ViewComponents where

import           Data.Text (Text)
import           Lucid
import Data.Monoid ((<>))

import Persons.Person

siteHeader :: Text -> Maybe Person -> Html ()
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
          case loggedIn of
           Just p ->
             "Welcome, " <> (toHtml $ pName p) <> a_ [href_ "/logout"] "Log out"
           Nothing -> a_ [href_ "/login"] "Log in"
