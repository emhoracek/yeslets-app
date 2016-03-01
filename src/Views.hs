{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Views where

import Lucid
import Data.Maybe (isJust)

import ViewComponents
import Signals.Signal
import Signals.Views
import Persons.Person
import Persons.Views

welcomeView :: [Signal] -> [Person] -> (Maybe Person) -> Html ()
welcomeView signals persons loggedIn = do
    siteHeader "Yeslets!" loggedIn
    if (isJust loggedIn) then
      div_ $ do
        h1_ "New Signal"
        form_ [action_ "/signal",
               method_ "POST"] $ do
          label_ [for_ "action"] $ "Action"
          input_ [ name_ "action"
                 , type_ "text"
                 , placeholder_ "send a signal"]
          br_ []
          label_ [ for_ "topic"] $ "Topic"
          input_ [ name_ "topic"
                 , type_ "text"
                 , placeholder_ "about what we should do together" ]
          br_ []
          input_ [ type_ "submit"]
    else mempty
    div_ $ do
      h1_ "Signals"
      signalsHtml (isJust loggedIn) signals
    div_ $ do
      h1_ "People"
      personsHtml persons
