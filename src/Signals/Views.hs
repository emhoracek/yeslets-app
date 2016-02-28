{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Signals.Views where

import Data.Serialize.Text ()
import Lucid
import Data.Monoid ((<>))

import Signals.Signal
import Persons.Person
import Utils

signalsHtml :: Bool -> [Signal] -> Html ()
signalsHtml loggedIn signals =
  foldr (<>) mempty (map (signalHtml loggedIn) signals)

signalHtml :: Bool -> Signal -> Html ()
signalHtml loggedIn signal =
  let person = sPerson signal
      name = a_ [href_ ("/persons/id" <> showT (pId person))]
               (toHtml $ pName person)
      act = toHtml $ sAction signal
      topic = toHtml $ sTopic signal
      yeslets = if loggedIn
                then yesletsesHtml $ signal
                else mempty in
  div_ [class_ "signal"] $
    p_ $ name <> (" wants to " <> act <> " " <> topic <> ". ") <> yeslets

yesletsesHtml :: Signal -> Html ()
yesletsesHtml signal =
  let ys = sYesletses signal
      yesletsUrl = "/signal/" <> showT (sId signal) <> "/yeslets" in
  div_ [class_ "yeslets"] $ do
    case ys of
     [] -> p_ (a_ [href_ yesletsUrl] "\"Yes, lets!\"")
     _  -> do
             ul_ (foldr (<>) mempty $ map yesletsHtml ys)
             p_ (a_ [href_ yesletsUrl] "\"Yes, lets!\"")

yesletsHtml :: Yeslets -> Html ()
yesletsHtml (Yeslets p) =
  li_ $ do
    a_ [href_ ("/persons/id/" <> (showT $ pId p))] (toHtml $ pName p)
