{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Persons.Views where

import           Lucid
import Data.Monoid ((<>))
import Persons.Person
import Utils

personsHtml :: [Person] -> Html ()
personsHtml persons = foldr (<>) mempty (map personHtml persons)

personHtml :: Person -> Html ()
personHtml person =
  div_ [class_ "person"] $
    p_ $ a_ [href_ ("/persons/id/" <> showT (pId person))]
            (toHtml $ pName person)
