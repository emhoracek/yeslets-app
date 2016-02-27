{-# LANGUAGE OverloadedStrings #-}

module Signals.Handlers where

import           Web.Fn
import           Network.Wai (Response)
import           Data.Monoid ((<>))

import Context
import ViewComponents
import Utils (lucidHtml)
import Signals.Signal
import Signals.Data
import Signals.Views

signalsHandler :: Ctxt -> IO (Maybe Response)
signalsHandler ctxt = do
  signals <- querySignals (_db ctxt)
  lucidHtml (siteHeader "signals" <> signalsHtml signals)

signalHandler :: Ctxt -> Int -> IO (Maybe Response)
signalHandler ctxt sId = do
  maybeSignal <- findSignalById sId (_db ctxt)
  let html s = lucidHtml (siteHeader "signals" <> signalHtml s)
  case maybeSignal of
   Just signal -> html signal
   Nothing -> return Nothing

addYesletsHandler :: Ctxt -> Int -> Int -> IO (Maybe Response)
addYesletsHandler ctxt sId pId = do
  rowsAffected <- addYeslets sId pId (_db ctxt)
  if rowsAffected == 1
  then okText "Yay! Yeslets added!"
  else okText "Couldn't add yeslets!"
