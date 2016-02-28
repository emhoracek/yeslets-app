{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Session.Views where

import           Lucid
import           ViewComponents (siteHeader)

loginView :: Bool -> Html ()
loginView loggedIn = do
  siteHeader "login" loggedIn
  h1_ "Login"
  form_ [action_ "/dologin",
         method_ "post"] $ do
    label_ [for_ "email"] $ do
      "Email"
      input_ [name_ "email",
              type_ "text"]
    label_ [for_ "password"] $ do
      "Password"
      input_ [name_ "password",
              type_ "password"]
    input_ [type_ "submit"]
