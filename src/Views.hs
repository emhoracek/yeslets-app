{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Views where

import Lucid

import ViewComponents
import Signals.Signal
import Signals.Views
import Persons.Person
import Persons.Views

welcomeView :: [Signal] -> [Person] -> Html ()
welcomeView signals persons = do
    siteHeader "Yeslets!"
    div_ $ do
      h1_ "Signals"
      signalsHtml signals
    div_ $ do
      h1_ "People"
      personsHtml persons
