{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Session.Handlers where

import           Web.Fn
import           Network.Wai (Response, vault)
import           Network.Wai.Session (withSession)
import qualified Database.PostgreSQL.Simple as PG
import           Data.Pool (Pool, withResource)
import           Data.Text (Text)
import           Data.Text.Read (decimal)
import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Vault.Lazy as Vault
import           Control.Lens ((^.), _1)
import qualified Database.Redis as R

import Session.Login
import ViewComponents
import Session.Views
import Persons.Person
import Context
import Utils

sessionHandler :: Ctxt -> IO (Maybe Response)
sessionHandler ctxt = do
  let Just (getSess, putSess) = Vault.lookup (ctxt ^. sess)
                                             (vault (ctxt ^. req ._1))
  current <- fromMaybe "0" <$> getSess "visits"
  let cur = case decimal current of
              Left _ -> error "Bad value in session"
              Right (n, _) -> n
  putSess "visit" (showT (cur + 1 :: Int))
  okText (showT cur)

doLoginHandler :: Ctxt -> Text -> Text -> IO (Maybe Response)
doLoginHandler ctxt email pass = do
  maybePerson <- tryLogin (Login email pass) (_db ctxt)
  case maybePerson of
    Just p -> route ctxt [ path "id" ==> (\ctxt -> okText (showT $ pId p))
                         , path "name" ==> (\ctxt -> okText (showT $ pName p))
                         , path "email" ==> (\ctxt -> okText (showT $ pEmail p))
                         , anything ==> (\ctxt -> okText (showT $ p)) ]
    Nothing -> okText "Ah! Ah! Ah! YOU DIDN'T SAY THE MAGIC WORD"

loginHandler :: Ctxt -> IO (Maybe Response)
loginHandler ctxt = lucidHtml $ loginView

tryLogin :: Login -> Pool PG.Connection -> IO (Maybe Person)
tryLogin login pgpool =
  withResource pgpool
    (\conn -> do
         let q = "SELECT id, name, email FROM persons WHERE email = ? AND password = crypt(?, password)"
         ps <- PG.query conn q (email login, password login) :: IO [Person]
         return $ listToMaybe ps)
