{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Session.Handlers where

import           Web.Fn
import           Network.Wai (Response, vault)
import qualified Database.PostgreSQL.Simple as PG
import           Data.Pool (Pool, withResource)
import           Data.Text (Text)
import           Data.Text.Read (decimal)
import           Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Vault.Lazy as Vault
import           Control.Lens ((^.), _1)

import Session.Login
import Session.Views
import Persons.Person
import Persons.Data
import Context
import Utils

sessionHandler :: Ctxt -> IO (Maybe Response)
sessionHandler ctxt =
  do let Just (getSess, putSess) = Vault.lookup (ctxt ^. sess)
                                                (vault (ctxt ^. req . _1))
     current <- fromMaybe "0" <$> getSess "visits"
     let cur = case decimal current of
                 Left _ -> error "Bad value in session"
                 Right (n,_) -> n
     putSess "visits" (showT (cur + 1 :: Int))
     okText (showT cur)

doLoginHandler :: Ctxt -> Text -> Text -> IO (Maybe Response)
doLoginHandler ctxt em pass = do
  maybePerson <- tryLogin (Login em pass) (_db ctxt)
  case maybePerson of
    Just p -> do
      let Just (_, putSess) = Vault.lookup (ctxt ^. sess)
                                           (vault (ctxt ^. req . _1))
      putSess "personId" (showT $ pId p)
      okText $ showT p
    Nothing -> okText "Ah! Ah! Ah! YOU DIDN'T SAY THE MAGIC WORD"

loginHandler :: Ctxt -> IO (Maybe Response)
loginHandler ctxt = do
  login <- loggedIn ctxt
  maybePerson <- case login of
                   Just l  -> findPersonById l (_db ctxt)
                   Nothing -> return Nothing
  lucidHtml $ loginView maybePerson

loggedIn :: Ctxt -> IO (Maybe Int)
loggedIn ctxt = do
  let Just (getSess, _) = Vault.lookup (ctxt ^. sess)
                                       (vault (ctxt ^. req . _1))
  maybeId <- getSess "personId"
  let pId' = case maybeId of
               Nothing -> Nothing
               Just n -> Just (readT n)
  return pId'

tryLogin :: Login -> Pool PG.Connection -> IO (Maybe Person)
tryLogin login pgpool =
  withResource pgpool
    (\conn -> do
         let q = "SELECT id, name, email FROM persons WHERE email = ? AND password = crypt(?, password)"
         ps <- PG.query conn q (email login, password login) :: IO [Person]
         return $ listToMaybe ps)
