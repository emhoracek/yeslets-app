{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Web.Fn
import           Network.Wai.Handler.Warp (run)
import           Control.Exception (SomeException, catch)
import           Data.Serialize.Text ()
import           Network.Wai (Application, Response)
import           Network.Wai.Session (withSession)
import           Network.Wai.Session.ClientSession (clientsessionStore)
import Web.ClientSession (randomKey)
import Data.Default (def)
import Data.Maybe (isJust)
import qualified Database.PostgreSQL.Simple as PG
import qualified Data.Vault.Lazy as Vault
import           Data.Pool (createPool, destroyAllResources)
import           Control.Lens ((^.))
import qualified Database.Redis as R
import           Control.Logging (withStdoutLogging, log')

import Context
import Views (welcomeView)
import Persons.Handlers
import Persons.Data
import Signals.Handlers
import Signals.Data
import Session.Handlers
import Utils (lucidHtml)

initializer :: IO Ctxt
initializer = do
  pgpool <- createPool (PG.connect (PG.ConnectInfo "localhost"
                                                   5432
                                                   "libby"
                                                   "123"
                                                   "yeslets"))
                        PG.close 1 60 20
  redis' <- R.connect R.defaultConnectInfo
  session <- Vault.newKey
  return (Ctxt defaultFnRequest pgpool redis' session)

site :: Ctxt -> IO Response
site ctxt = do
  maybeLoginId <- loggedIn ctxt
  putStrLn $ "Request says: " ++ (show maybeLoginId)
  route ctxt [ end ==> welcomeHandler
             , path "signals" ==> signalsHandler
             , path "signal" // segment // end ==> signalHandler
             , path "signal" // segment //
                 path "yeslets" // end ==> yesletsHandler
             , path "persons" ==> personsHandler
             , path "session" ==> sessionHandler
             , path "login" // end ==> loginHandler
             , path "dologin" /? param "email"
                              /? param "password" ==> doLoginHandler
             , anything ==> staticServe "static" ]
    `fallthrough` notFoundText "Page not found."

welcomeHandler :: Ctxt -> IO (Maybe Response)
welcomeHandler ctxt = do
  signals <- querySignals (_db ctxt)
  persons <- queryPersons (_db ctxt)
  isLoggedIn <- loggedIn ctxt
  lucidHtml $ welcomeView signals persons (isJust isLoggedIn)

app :: IO (Application, IO())
app = do
  -- don't do this???
  (_, k) <- randomKey
  let store = clientsessionStore k
  ctxt <- initializer
  return (withSession store "_session" def (ctxt ^. sess) (toWAI ctxt site)
         ,destroyAllResources (ctxt ^. db))

-- Run initialized app on port 8000
main :: IO ()
main = withStdoutLogging $ do
  log' $ "Starting server on port 8000..."
  (app', shutdown) <- app
  catch (run 8000 app')
        (\(_ :: SomeException) -> shutdown)
