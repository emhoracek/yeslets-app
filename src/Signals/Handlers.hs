{-# LANGUAGE OverloadedStrings #-}

module Signals.Handlers where

import           Web.Fn
import           Network.Wai (Response)
import           Data.Monoid ((<>))
import Data.Text (Text)
import Data.Maybe (isJust)

import Context
import ViewComponents
import Utils (lucidHtml)
import Persons.Person
import Signals.Signal
import Signals.Data
import Signals.Views
import Session.Handlers

signalsHandler :: Ctxt -> IO (Maybe Response)
signalsHandler ctxt = do
  login <- loggedIn ctxt
  signals <- querySignals (_db ctxt)
  lucidHtml (signalsHtml (isJust login) signals)

signalHandler :: Ctxt -> Int -> IO (Maybe Response)
signalHandler ctxt sId' = do
  login <- loggedIn ctxt
  maybeSignal <- findSignalById sId' (_db ctxt)
  let html s = lucidHtml (signalHtml (isJust login) s)
  case maybeSignal of
   Just signal -> html signal
   _  -> return Nothing

newSignalHandler :: Ctxt -> Text -> Text -> IO (Maybe Response)
newSignalHandler ctxt action topic = do
  maybeLogin <- loggedIn ctxt
  case maybeLogin of
    Just login -> do
      rowsAffected <- addSignal action topic login (_db ctxt)
      if rowsAffected == 1
      then okText "Added!"
      else okText "Nope"
    Nothing -> okText "You can't do that."

yesletsHandler :: Ctxt -> Int -> IO (Maybe Response)
yesletsHandler ctxt sId' = do
  maybeLogin <- loggedIn ctxt
  maybeSignal <- findSignalById sId' (_db ctxt)
  case maybeSignal of
    Just signal -> do
      let pIds = map (\(Yeslets p) -> pId p) (sYesletses signal)
      case maybeLogin of
        Just login -> do
          rowsAffected <- if login `elem` pIds
                          then removeYeslets (sId signal) login (_db ctxt)
                          else addYeslets (sId signal) login (_db ctxt)
          if rowsAffected == 1
            then okText "OKAY"
            else error "something went wrong"
        Nothing -> okText "you can't do that"
    Nothing -> okText "no such signal"
