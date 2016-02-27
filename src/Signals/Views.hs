{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signals.Views where

import           Web.Fn
import           Network.Wai.Handler.Warp (run)
import Control.Exception (SomeException, catch)
import Data.Serialize.Text ()
import           Network.Wai (Application, Request,
                              Response, defaultRequest, vault)
import           Network.Wai.Session (Session, withSession)
import           Network.Wai.Session.ClientSession (clientsessionStore)
import Web.ClientSession (randomKey)
import Data.Default (def)
import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.FromRow
import           Data.Text (Text, pack)
import qualified Data.Text as T
--import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import qualified Data.Vault.Lazy as Vault
import           Data.Text.Lazy (toStrict)
import           Data.Pool (Pool, createPool, withResource, destroyAllResources)
import           Control.Lens (makeLenses, (^.), _1)
import           Data.Maybe (fromJust, listToMaybe, fromMaybe)
import           Lucid
import qualified Database.Redis as R
import Data.Monoid ((<>), mempty)
import Data.Int (Int64)
import Signals.Signal
import Persons.Person
import Utils
import ViewComponents

signalsHtml :: [Signal] -> Html ()
signalsHtml signals = foldr (<>) mempty (map signalHtml signals)

signalHtml :: Signal -> Html ()
signalHtml signal =
  let person = sPerson signal
      name = a_ [href_ ("/persons/id" <> showT (pId person))] (toHtml $ pName person)
      act = sAction signal
      topic = sTopic signal
      yeslets = yesletsesHtml $ signal in
  div_ [class_ "signal"] $
    p_ $ name <> (toHtml $ " wants to " <> act <> " " <> topic <> ". ") <> yeslets

yesletsesHtml :: Signal -> Html ()
yesletsesHtml signal =
  let ys = sYesletses signal
      yesletsUrl = "/signal/" <> showT (sId signal) <> "/yeslets" in
  div_ [class_ "yeslets"] $ do
    case ys of
     [] -> p_ ((toHtml "Be the first to say, ") <>
               (a_ [href_ yesletsUrl] "\"Yes, lets!\""))
     _  -> do
             ul_ (foldr (<>) mempty $ map yesletsHtml ys)
             p_ (a_ [href_ yesletsUrl] "\"Yes, lets!\"")

yesletsHtml :: Yeslets -> Html ()
yesletsHtml (Yeslets p) =
  li_ $ do
    a_ [href_ ("/persons/id" <> (showT $ pId p))] (toHtml $ pName p)
